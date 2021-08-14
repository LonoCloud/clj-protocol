(ns protocol.fields
  "Read, write, and manipulate field data.

  Reader functions take `[buf start ctx]` and return `[end
  value-or-values]` where `end` is the offset in `buf` after the read
  value(s).

  Writer functions take `[buf value-or-values start ctx]` and return
  `end` where `end` is the offset in `buf` after the written
  value(s)."
  (:require [protocol.platform :as plat]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field and buffer manipulation functions

(defn octet->int
  "Convert sequence of octets/bytes `octets` (in MSB first order) into
  an integer"
  [octets]
  (reduce (fn [a o] (+ o (* a 256))) octets))

(defn int->octet
  "Convert integer `n` into `cnt` octets/bytes (in MSB first order)"
  [n cnt]
  (vec (first
         (reduce (fn [[res x] _] [(conj res (bit-and x 255)) (quot x 256)])
                 [(list) n]
                 (range cnt)))))

(defn int->hex
  "Convert integer `i` into hex string representation"
  [i]
  (let [h (plat/num->string i 16)]
    (if (= 1 (count h)) (str "0" h) h)))

;;;

(defn list->lookup
  "Takes columnar collection and one or more [k-idx v-idx] pairs and
  returns a map contructed from key/value pairs selected from
  `k-idx`/`v-idx` columns of `coll`. If `v-idx` does not exist for
  a given row then the value defaults to nil."
  [coll & idxs]
  (reduce (fn [m [ki vi]]
            (merge m (zipmap (map #(nth % ki)     coll)
                             (map #(nth % vi nil) coll))))
          {}
          idxs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compound reader and writer functions

(defn read-lookup
  "Read `lookup-type` type from `buf` at `start` and lookup that key
  in `lookup` to get the value to return"
  [buf start {:keys [readers lookup-type lookup] :as ctx}]
  (let [reader (readers lookup-type)
        _ (assert reader (str "Unknown lookup type " lookup-type))
        [end k] (reader buf start ctx)
        v (get lookup k)]
    (assert v (str "Key " k " not found in lookup"))
    [end v]))

(defn write-lookup
  "Lookup `k` in `lookup` and write it as a `lookup-type` type into
  `buf` at `start`"
  [buf k start {:keys [writers lookup-type lookup] :as ctx}]
  (let [writer (writers lookup-type)
        _ (assert writer (str "Unknown lookup type " lookup-type))
        v (get lookup k)]
    (assert v (str "Key " k " not found in lookup"))
    (writer buf v start ctx)))

;;;

(defn read-repeat
  "Read a sequence of `repeat-type` elements from `buf` starting at
  `start`. The number of elements read is `length` / `repeat-size`"
  [buf start {:keys [readers length repeat-type repeat-size] :as ctx}]
  (assert repeat-type "No repeat-type specified")
  (assert length "No length given for repeat")
  (let [end (+ start length)
        reader (readers repeat-type)]
    (assert reader (str "No reader found for " repeat-type))
    [end (map #(second (reader buf % ctx))
              (range start end repeat-size))]))

(defn write-repeat
  "Write a sequence of `repeat-type` elements (of size `repeat-size`)
  into `buf` starting at `start`"
  [buf values start {:keys [writers repeat-type repeat-size] :as ctx}]
  (assert repeat-type (str "No repeat-type specified"))
  (let [writer (writers repeat-type)]
    (assert writer (str "No writer found for " repeat-type))
    (last (map (fn [v o] (writer buf v o ctx))
               values
               (iterate #(+ % repeat-size) start)))))


(defn read-loop
  "Read a sequence of `loop-type` elements from `buf` starting at
  `start`. Elements will be read until either length is reached, the
  end of the buffer is reached, or the `loop-type` reader returns
  a three-tuple `[end value stop?]` with a truthy `stop?` value."
  [buf start {:keys [readers loop-type length] :as ctx}]
  ;;(prn :read-loop0 :loop-type loop-type :start start :length length)
  (assert loop-type "No loop-type specified")
  (let [end (if length (+ start length) (plat/buf-len buf))
        reader (get readers loop-type)]
    (assert reader (str "No loop reader for " loop-type))
    (loop [offset start
           res []]
      ;;(prn :read-loop1 :offset offset :end end :res res)
      (if (>= offset end)
        [offset res]
        (let [ctx (assoc ctx :length (- end offset))
              [fend value stop?] (reader buf offset ctx)
              res (conj res value)]
          ;;(prn :read-loop2 :offset offset :end end :fend fend :value value)
          (if stop?
            [fend res]
            (recur fend res)))))))

(defn write-loop
  "Write a sequence of `loop-type` elements into `buf` starting at
  `start`. All elements in `values` will be written to the buffer."
  [buf values start {:keys [writers loop-type] :as ctx}]
  ;;(prn :write-loop loop-type :start start :values values)
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

;;;

(defn read-choice
  "Read using a reader and context that is selected based on
  a previously read value. The `choice-path` vector is looked up in
  `msg-map` (using `get-in`) to get a choice key. The choice key is
  looked up in `choices` to get a choice context map. The choice map
  must contain a `:choice-type` that specifies the selected reader.
  Any other entries in the choice context map are merged into the new
  context for that reader."
  [buf start {:keys [msg-map readers choice-path choices] :as ctx}]
  (let [choice-key (get-in msg-map choice-path)
        _ (assert choice-key
                  (str "Switch path " choice-path " not in msg-map"))
        choice (get choices choice-key)
        _ (assert choice
                  (str "Switch value " choice-key " not in choices map"))
        {:keys [choice-type spec]} choice
        reader (readers choice-type)]
    (assert reader (str "No reader for " choice-type))
    (reader buf start (merge ctx (dissoc choice choice-type)))))

(defn write-choice
  "Write using a writer and context that is selected based on
  another value in `msg-map`. The `choice-path` vector is looked up in
  `msg-map` (using `get-in`) to get a choice key. The choice key is
  looked up in `choices` to get a choice context map. The choice map
  must contain a `:choice-type` that specifies the selected writer.
  Any other entries in the choice context map are merged into the new
  context for that writer."
  [buf value start {:keys [msg-map writers choice-path choices] :as ctx}]
  (let [choice-key (get-in msg-map choice-path)
        _ (assert choice-key
                  (str "Switch path " choice-path " not in msg-map"))
        choice (get choices choice-key)
        _ (assert choice
                  (str "Switch value " choice-key " not in choices map"))
        {:keys [choice-type spec]} choice
        writer (writers choice-type)]
    (assert writer (str "No writer for " choice-type))
    (writer buf value start (merge ctx (dissoc choice choice-type)))))

;;;

(defn read-bitfield
  "Read `length` bytes from `buf` starting at `start` and return a map
  of bitfields based on those bytes and the bitfield `spec`. The
  bitfield `spec` is a sequence `[name type bits]` entries where
  `name` is the bitfield key name, `type` can be either `:int` or `:bool` (for
  integer and boolean bitfield respectively), and `bits` is the number
  of bits to decode for that bitfield."
  [buf start {:keys [length spec le?] :as ctx}]
  (assert length "Bitfield length not specified")
  (assert spec "Bitfield spec not specified")
  (let [end (+ start length)
        byts (plat/buf->vec buf start end)
        byts (if le? (reverse byts) byts)
        bits (plat/bytes->bits byts)]
    [end
     (first
       (reduce (fn [[res bs] [nam typ len]]
                 ;;(prn :res res :bs bs :name nam :typ typ :len len)
                 (let [i (plat/string->num (apply str (take len bs)) 2)]
                   [(assoc res nam (condp = typ :int i :bool (> i 0)))
                    (drop len bs)]))
               [{} bits] spec))]))

(defn- read-bitfield-BE [b s c] (read-bitfield b s (assoc c :le? false)))
(defn- read-bitfield-LE [b s c] (read-bitfield b s (assoc c :le? true)))

(defn write-bitfield
  "Write the bitfield values from `bf-map` into `buf` starting at
  `start` and based on the bitfield `spec`. The bitfield `spec` is
  a sequence `[name type bits]` entries where `name` is the bitfield
  key in `bf-map`, `type` can be either `:int` or `:bool` (for integer
  and boolean bitfield respectively), and `bits` is the number of bits
  to encode for that bitfield."
  [buf bf-map start {:keys [spec le?] :as ctx}]
  (assert spec "Bitfield spec not specified")
  (let [byts (plat/bits->bytes
               (reduce (fn [res [nam typ len]]
                         (let [i (let [i (get bf-map nam 0)]
                                   (get {true 1 false 0} i i))
                               bs (take-last len (concat (repeat len "0")
                                                         (plat/num->string i 2)))]
                           (into res bs)))
                       [] spec))
        byts (if le? (reverse byts) byts)]
    (plat/arr-fill buf byts start)))

(defn- write-bitfield-BE [b v s c] (write-bitfield b v s (assoc c :le? false)))
(defn- write-bitfield-LE [b v s c] (write-bitfield b v s (assoc c :le? true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Readers and writers

;; Called with [buf start ctx]
;; Returns [offset value]
(def ^:private readers-base
  {:buf       #(let [e (+ %2 (or (:length %3) 0)) v (plat/buf-slice %1 %2 e)] [e v])
   :raw       #(let [e (+ %2 (or (:length %3) 0)) v (plat/buf->vec %1 %2 e)] [e v])
   :utf8      #(let [[e v] (plat/buf-read-utf8 %1 %2 (or (:length %3) 0))]
                 [e (plat/remove-nulls v)])
   :uint8     (fn [b s c] (plat/buf-read-uint8 b s))
   :lookup    read-lookup
   :repeat    read-repeat
   :loop      read-loop
   :choice    read-choice})

(def ^{:doc "Field readers (big endian)"}
  readers-BE
  (merge readers-base
         {:uint16    (fn [b s c] (plat/buf-read-uint16-be b s))
          :uint32    (fn [b s c] (plat/buf-read-uint32-be b s))
          :uint64    (fn [b s c] (plat/buf-read-uint64-be b s))
          :bitfield  read-bitfield-BE}))

(def ^{:doc "Field readers (little endian)"}
  readers-LE
  (merge readers-base
         {:uint16    (fn [b s c] (plat/buf-read-uint16-le b s))
          :uint32    (fn [b s c] (plat/buf-read-uint32-le b s))
          :uint64    (fn [b s c] (plat/buf-read-uint64-le b s))
          :bitfield  read-bitfield-LE}))

;; Called with [buf value start ctx]
;; Returns offset/end after written value
(def ^:private writers-base
  {:buf       (fn [b v s c] (plat/buf-fill b v s))
   :raw       (fn [b v s c] (plat/arr-fill b v s))
   :utf8      #(let [l (or (:length %4) (count %2))]
                 (do (plat/buf-write-utf8 %1 %2 %3 l) (+ l %3)))
   :uint8     (fn [b v s c] (plat/buf-write-uint8 b v s))
   :lookup    write-lookup
   :repeat    write-repeat
   :loop      write-loop
   :choice    write-choice})

(def ^{:doc "Field writers (big endian)"}
  writers-BE
  (merge writers-base
         {:uint16    (fn [b v s c] (plat/buf-write-uint16-be b v s))
          :uint32    (fn [b v s c] (plat/buf-write-uint32-be b v s))
          :uint64    (fn [b v s c] (plat/buf-write-uint64-be b v s))
          :bitfield  write-bitfield-BE}))

(def ^{:doc "Field writers (little endian)"}
  writers-LE
  (merge writers-base
         {:uint16    (fn [b v s c] (plat/buf-write-uint16-le b v s))
          :uint32    (fn [b v s c] (plat/buf-write-uint32-le b v s))
          :uint64    (fn [b v s c] (plat/buf-write-uint64-le b v s))
          :bitfield  write-bitfield-LE}))
