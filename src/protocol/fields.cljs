(ns protocol.fields)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field and buffer manipulation functions

(defn octet->int [os]
  (reduce (fn [a o] (+ o (* a 256))) os))

(defn int->octet [n cnt]
  (vec (first
         (reduce (fn [[res x] _] [(conj res (bit-and x 255)) (quot x 256)])
                 [(list) n]
                 (range cnt)))))

(defn int->hex [i]
  (let [h (js/Number.prototype.toString.call i 16)]
    (if (= 1 (.-length h)) (str "0" h) h)))

;;;

(defn bytes->bits [byts]
  (mapcat #(map js/parseInt
                (take-last 8 (seq (str "00000000" (.toString % 2)))))
          byts))

(defn bits->bytes [bits]
  (map #(js/parseInt (apply str %) 2)
       (partition 8 bits)))

(defn arr-fill [dbuf arr off & [cnt]]
  (let [tend (+ off (or cnt (count arr)))]
    (.fill dbuf (.from js/Buffer (clj->js arr)) off tend)
    tend))

(defn buf-fill [dbuf sbuf off]
  (let [tend (+ off (.-length sbuf))]
    (.fill dbuf sbuf off tend)
    tend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compound reader and writer functions

(defn bytes->bitfield [byts spec]
  (let [bits (bytes->bits byts)]
    (first
      (reduce (fn [[res bs] [nam typ len]]
                ;;(prn :res res :bs bs :name nam :typ typ :len len)
                (let [i (js/parseInt (apply str (take len bs)) 2)]
                  [(assoc res nam (condp = typ :int i :bool (> i 0)))
                   (drop len bs)]))
              [{} bits] spec))))

(defn bitfield->bytes [bmap spec]
  (bits->bytes
    (reduce (fn [res [nam typ len]]
              (let [i (let [i (get bmap nam 0)] (get {true 1 false 0} i i))
                    bs (take-last len (concat (repeat len "0")
                                              (.toString i 2)))]
                (into res bs)))
            [] spec)))

(defn read-repeat
  [buf start end {:keys [readers repeat-type repeat-size] :as ctx}]
  (assert repeat-type "No repeat-type specified")
  (let [reader (readers repeat-type)]
  (assert reader (str "No reader specified"))
    (map #(reader buf % (+ % repeat-size) ctx)
         (range start end repeat-size))))

(defn write-repeat
  [buf values start {:keys [writers repeat-type repeat-size] :as ctx}]
  (assert repeat-type (str "No repeat-type specified"))
  (let [writer (writers repeat-type)]
    (assert writer (str "No writer specified"))
    (last (map (fn [v o] (writer buf v o ctx))
               values
               (iterate #(+ % repeat-size) start)))))


(defn read-loop
  [buf start end {:keys [readers loop-type] :as ctx}]
  ;;(prn :read-loop loop-type)
  (assert loop-type "No loop-type specified")
  (let [reader (get readers loop-type)]
    (assert reader (str "No loop reader for " loop-type))
    (loop [offset start
           res []]
      (if (>= offset end)
        res
        (let [value (reader buf offset end ctx)
              val-meta (meta value)
              fend (:protocol/end val-meta)
              ;; Hoist metadata
              res (vary-meta (conj res value) merge val-meta)]
          (assert fend "Reader did not have :protocol/end metadata")
          (if (:protocol/stop val-meta)
            res
            (recur fend res)))))))

(defn write-loop
  [buf values start {:keys [writers loop-type] :as ctx}]
  ;;(prn :write-loop loop-type)
  (assert loop-type "No loop-type specified")
  (let [writer (get writers loop-type)]
    (assert writer (str "No loop writer for " loop-type))
    (loop [fend start
           values values]
      (if (not (seq values))
        fend
        (let [value (first values)
              fend (writer buf value fend ctx)]
          (recur fend (next values)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Readers and writers

(def remove-null-re (js/RegExp. "\u0000" "g"))

(set! *warn-on-infer* false)

;; Called with [buf start end ctx]
;; Return value read
(def readers-base
  {:buf       #(.slice %1 %2 %3)
   :raw       #(vec (.slice %1 %2 %3))
   :str       #(.replace (.toString %1 "utf8" %2 %3) remove-null-re "")
   :uint8     #(.readUInt8 %1 %2)
   :repeat    read-repeat
   :loop      read-loop})

(def readers-BE
  (merge
    readers-base
    {:uint16    #(.readUInt16BE %1 %2)
     :uint32    #(.readUInt32BE %1 %2)
     :uint64    #(.readBigUInt64BE %1 %2)
     :bitfield  #(bytes->bitfield (vec (.slice %1 %2 %3)) (:spec %4))}))

(def readers-LE
  (merge
    readers-base
    {:uint16    #(.readUInt16LE %1 %2)
     :uint32    #(.readUInt32LE %1 %2)
     :uint64    #(.readBigUInt64LE %1 %2)
     :bitfield  #(bytes->bitfield (vec (reverse (.slice %1 %2 %3))) (:spec %4))}))

;; Called with [buf value start ctx]
;; Returns offset/end after written value
(def writers-base
  {:buf       #(buf-fill %1 %2 %3)
   :raw       #(arr-fill %1 %2 %3)
   :str       #(do (.write %1 %2 %3 "utf8") (+ (.-length %2) %3))
   :uint8     #(.writeUInt8 %1 %2 %3)
   :repeat    write-repeat
   :loop      write-loop})

(def writers-BE
  (merge
    writers-base
    {:uint16    #(.writeUInt16BE %1 %2 %3)
     :uint32    #(.writeUInt32BE %1 %2 %3)
     :uint64    #(.writeBigUInt64BE %1 %2 %3)
     :bitfield  #(arr-fill %1 (bitfield->bytes %2 (:spec %4)) %3)}))

(def writers-LE
  (merge
    writers-base
    {:uint16    #(.writeUInt16LE %1 %2 %3)
     :uint32    #(.writeUInt32LE %1 %2 %3)
     :uint64    #(.writeBigUInt64LE %1 %2 %3)
     :bitfield  #(arr-fill %1 (reverse (bitfield->bytes %2 (:spec %4))) %3)}))
