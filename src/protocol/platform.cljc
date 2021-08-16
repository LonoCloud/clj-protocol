(ns protocol.platform
  "Protocol platform specific functions"
  (:require [clojure.string :as string]
            #?(:clj [clojure.pprint :refer [cl-format]])
            #?(:clj [clojure.java.io :as io])
            #?(:cljs ["fs" :as fs]))
  #?(:clj
     (:import [java.math BigInteger]
              [java.nio ByteBuffer ByteOrder]
              [java.nio.charset StandardCharsets])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous definitions

(def Err #?(:cljs js/Error :clj java.lang.Exception))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string and number manipulation

(defn big-int [s]
  #?(:cljs (js/BigInt s)
     :clj  (BigInteger. s)))

(defn num->string [n base]
  #?(:cljs  (.toString n base)
     :clj   (Long/toString n base)))

(defn string->num
  ([s] (string->num s 10))
  ([s base] #?(:cljs  (js/parseInt s base)
               :clj   (Long/parseLong s base))))

(defn pr-num
  "Print a number `n` in base `base` with 0 padding up to size `sz`."
  [n base sz]
  #?(:cljs  (.padStart (num->string n base) sz "0")
     :clj   (cl-format nil (str "~" sz ",'0d") (num->string n base))))

(defn bytes->bits
  "Convert a sequence of bytes/octets `byts` (in MSB first order) into
  a sequence of 0/1 'bits' (in MSB first order)"
  [byts]
  (mapcat #(map string->num
                (take-last 8 (string/split (str "00000000" (num->string % 2))
                                           #"")))
          byts))

(defn bits->bytes
  "Convert a sequence of 0/1 'bits' (in MSB first order) into
  a sequence of bytes (in MSB first order)"
  [bits]
  (map #(string->num (apply str %) 2)
       (partition 8 bits)))

(def ^:private remove-null-re
  #?(:cljs (js/RegExp. "\u0000" "g")
     :clj  #"\u0000"))

(defn remove-nulls [s]
  #?(:cljs (.replace s remove-null-re "")
     :clj  (string/replace s remove-null-re "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer manipulation

(defn buf-alloc [sz]
  #?(:cljs (.alloc js/Buffer sz)
     :clj  (ByteBuffer/allocate sz)))

(defn buf-len [buf]
  #?(:cljs (.-length buf)
     :clj  (.capacity buf)))

(defn buf-cmp [buf1 buf2]
  #?(:cljs (.compare buf1 buf2)
     :clj  (do
             (.position buf1 0)
             (.position buf2 0)
             (.compareTo buf1 buf2))))

(defn buf-from [arr]
  #?(:cljs (.from js/Buffer (clj->js arr))
     :clj  (let [buf (buf-alloc (count arr))]
             (.put buf (byte-array arr) 0 (count arr)))))

(defn arr-fill
  "Write/fill an sequence of octets/bytes from `arr` into the buffer
  `dbuf` starting at `off`. If `cnt` is specified then only the first
  `cnt` octets are written"
  [dbuf arr off & [cnt]]
  (let [len (or cnt (count arr))
        tend (+ off len)]
    #?(:cljs (.fill dbuf (.from js/Buffer (clj->js arr)) off tend)
       :clj  (do (.position dbuf off)
                 (.put dbuf (byte-array arr) 0 len)))
    tend))

(defn buf-fill
  "Copy/fill all bytes from `sbuf` into `dbuf` starting at offset
  `off` within `dbuf`"
  [dbuf sbuf off]
  (let [tend (+ off (buf-len sbuf))]
    #?(:cljs (.fill dbuf sbuf off tend)
       :clj  (do (.position dbuf off)
                 (.put dbuf sbuf 0 (.capacity sbuf))))
    tend))

(defn buf-slice
  ([buf off] (buf-slice buf off (buf-len buf)))
  ([buf off end]
   #?(:cljs (.slice buf off end)
      :clj  (let [len (- end off)
                  buf2 (buf-alloc len)
                  l (.limit buf)] ;; save limit
              (.position buf off)
              (.limit buf (+ off len))
              (.put buf2 buf)
              (.limit buf l) ;; restore limit
              buf2))))

(defn buf->vec
  "Slice buffer `buf` starting at `off` and ending at `end` and return
  a vector of the octets/bytes"
  ([buf off] (buf->vec buf off (buf-len buf)))
  ([buf off end]
   #?(:cljs (-> (.slice buf off end) (.toJSON) (.-data) vec)
      :clj  (let [len (- end off)
                  ba (byte-array len)]
              (.position buf off)
              (.get buf ba 0 len)
              (vec (map #(bit-and 0xff %) ba))))))

(defn buf-slurp [path]
  #?(:cljs (.readFileSync fs path)
     :clj (with-open [is (io/input-stream path)
                      baos (java.io.ByteArrayOutputStream.)]
            (let [bb (buf-alloc (.available is))]
              (io/copy is baos)
              (buf-from (.toByteArray baos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer readers/writers

;; All readers return [e value] where e is the offset after the read
;; data and value is the read data.
;; All writers return e; the offset after the written data

#?(:cljs (set! *warn-on-infer* false))

;; https://stackoverflow.com/questions/88838/how-to-convert-strings-to-and-from-utf8-byte-arrays-in-java
(defn buf-read-utf8
  [buf off length]
  (let [end (+ off length)
        s #?(:cljs (.toString buf "utf8" off end)
             :clj  (let [l (.limit buf)] ;; save limit
                     (.position buf off)
                     (.limit buf (+ off length))
                     (let [s (.toString (.decode StandardCharsets/UTF_8 buf))]
                       (.limit buf l) ;; restore limit
                       s)))]
    [end s]))

(defn buf-write-utf8
  [buf s off length]
  (let [end (+ off length)
        s #?(:cljs (.write buf s off length "utf8")
             :clj  (let [bb (.encode StandardCharsets/UTF_8 s)]
                     (.position buf off)
                     (.put buf bb)))]
    end))

(defn buf-read-uint8 [buf off]
  (vector (+ off 1)
          #?(:cljs (.readUInt8 buf off)
             :clj  (bit-and 0xff (.get buf off)))))

(defn buf-write-uint8 [buf v off]
  #?(:cljs (.writeUInt8 buf v off)
     :clj  (do (.put buf off v) (+ off 1))))

(defn buf-read-uint16-be [buf off]
  (vector (+ off 2)
          #?(:cljs (.readUInt16BE buf off)
             :clj  (do (.order buf ByteOrder/BIG_ENDIAN)
                       (bit-and 0xFFFF (int (.getShort buf off)))))))
(defn buf-read-uint16-le [buf off]
  (vector (+ off 2)
          #?(:cljs (.readUInt16LE buf off)
             :clj  (do (.order buf ByteOrder/LITTLE_ENDIAN)
                       (bit-and 0xFFFF (int (.getShort buf off)))))))

(defn buf-write-uint16-be [buf v off]
  #?(:cljs (.writeUInt16BE buf v off)
     :clj  (do (.order buf ByteOrder/BIG_ENDIAN)
               (.putShort buf off v) (+ off 2))))
(defn buf-write-uint16-le [buf v off]
  #?(:cljs (.writeUInt16LE buf v off)
     :clj  (do (.order buf ByteOrder/LITTLE_ENDIAN)
               (.putShort buf off v) (+ off 2))))

(defn buf-read-uint32-be [buf off]
  (vector (+ off 4)
          #?(:cljs (.readUInt32BE buf off)
             :clj  (do (.order buf ByteOrder/BIG_ENDIAN)
                       (bit-and 0xFFFFFFFF (long (.getInt buf off)))))))
(defn buf-read-uint32-le [buf off]
  (vector (+ off 4)
          #?(:cljs (.readUInt32LE buf off)
             :clj  (do (.order buf ByteOrder/LITTLE_ENDIAN)
                       (bit-and 0xFFFFFFFF (long (.getInt buf off)))))))

(defn buf-write-uint32-be [buf v off]
  #?(:cljs (.writeUInt32BE buf v off)
     :clj  (do (.order buf ByteOrder/BIG_ENDIAN)
               (.putInt buf off v) (+ off 4))))
(defn buf-write-uint32-le [buf v off]
  #?(:cljs (.writeUInt32LE buf v off)
     :clj  (do (.order buf ByteOrder/LITTLE_ENDIAN)
               (.putInt buf off v) (+ off 4))))

(defn buf-read-uint64-be [buf off]
  (vector (+ off 8)
          #?(:cljs (.readBigUInt64BE buf off)
             :clj  (do (.order buf ByteOrder/BIG_ENDIAN)
                       (.getLong buf off)))))
(defn buf-read-uint64-le [buf off]
  (vector (+ off 8)
          #?(:cljs (.readBigUInt64LE buf off)
             :clj  (do (.order buf ByteOrder/LITTLE_ENDIAN)
                       (.getLong buf off)))))

(defn buf-write-uint64-be [buf v off]
  #?(:cljs (.writeBigUInt64BE buf v off)
     :clj  (do (.order buf ByteOrder/BIG_ENDIAN)
               (.putLong buf off v) (+ off 8))))
(defn buf-write-uint64-le [buf v off]
  #?(:cljs (.writeBigUInt64LE buf v off)
     :clj  (do (.order buf ByteOrder/LITTLE_ENDIAN)
               (.putLong buf off v) (+ off 8))))

