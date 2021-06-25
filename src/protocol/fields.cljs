(ns protocol.fields)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field data manipulation functions

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

(defn bytes->bits [byts]
  (mapcat #(map js/parseInt
                (take-last 8 (seq (str "00000000" (.toString % 2)))))
          byts))

(defn bits->bytes [bits]
  (map #(js/parseInt (apply str %) 2)
       (partition 8 bits)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field readers and writers

(set! *warn-on-infer* false)

(def remove-null-re (js/RegExp. "\u0000" "g"))

;; Called with [buf start end readers arg/lookup]
;; Return value read
(def readers
  {:buf       #(.slice %1 %2 %3)
   :raw       #(vec (.slice %1 %2 %3))
   :str       #(.replace (.toString %1 "utf8" %2 %3) remove-null-re "")
   :uint8     #(.readUInt8 %1 %2)
   :uint16    #(.readUInt16BE %1 %2)
   :uint32    #(.readUInt32BE %1 %2)
   :uint64    #(.readBigUInt64BE %1 %2)
   :ipv4      #(vec (.slice %1 %2 (+ 4 %2)))
   :mac       #(vec (.slice %1 %2 (+ 6 %2)))
   :bitfield  #(bytes->bitfield (vec (.slice %1 %2 %3)) %5)})

(defn arr-fill [dbuf arr off & [cnt]]
  (let [tend (+ off (or cnt (count arr)))]
    (.fill dbuf (.from js/Buffer (clj->js arr)) off tend)
    tend))

(defn buf-fill [dbuf sbuf off]
  (let [tend (+ off (.-length sbuf))]
    (.fill dbuf sbuf off tend)
    tend))

;; Called with [buf value start writers arg/lookup]
;; Returns offset/end after written value
(def writers
  {:buf       #(buf-fill %1 %2 %3)
   :raw       #(arr-fill %1 %2 %3)
   :str       #(do (.write %1 %2 %3 "utf8") (+ (.-length %2) %3))
   :uint8     #(.writeUInt8 %1 %2 %3)
   :uint16    #(.writeUInt16BE %1 %2 %3)
   :uint32    #(.writeUInt32BE %1 %2 %3)
   :uint64    #(.writeBigUInt64BE %1 %2 %3)
   :ipv4      #(arr-fill %1 %2 %3 4)
   :mac       #(arr-fill %1 %2 %3 6)
   :bitfield  #(arr-fill %1 (bitfield->bytes %2 %5) %3)})

