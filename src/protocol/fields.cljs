(ns protocol.fields
  (:require [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network Address support functions

(defn octet->int [os]
  (reduce (fn [a o] (+ o (* a 256))) os))

(defn int->octet [n cnt]
  (vec (first
         (reduce (fn [[res x] _] [(conj res (bit-and x 255)) (quot x 256)])
                 [(list) n]
                 (range cnt)))))

(defn ip->octet [s]
  (map int (.split s ".")))

(defn octet->ip [o]
  (string/join "." o))

(defn mac->octet [s]
  (map #(js/parseInt %1 16) (.split s ":")))

(defn int->hex [i]
  (let [h (js/Number.prototype.toString.call i 16)]
    (if (= 1 (.-length h)) (str "0" h) h)))

(defn octet->mac [o]
  (string/join ":" (map int->hex o)))

(defn ip->int [ip]
  (octet->int (ip->octet ip)))

(defn int->ip [int]
  (octet->ip (int->octet int 4)))

(defn first-ip [ip netmask]
  (octet->ip
   (map #(bit-and %1 %2) (ip->octet ip) (ip->octet netmask))))

(defn broadcast [ip netmask]
  (octet->ip
   (map #(bit-or %1 %2)
        (ip->octet ip)
        (map #(+ 256 (bit-not %1)) (ip->octet netmask)))))

(defn mask-int->prefix [mask-int]
  (let [bin (.toString mask-int 2)]
    (count (filter #(= "1" %) (.split bin "")))))

(defn mask-ip->prefix [mask-ip]
  (mask-int->prefix (ip->int mask-ip)))

(defn network-start-end [ip netmask & [usable?]]
  (let [start (ip->int (first-ip ip netmask))
        end (ip->int (broadcast ip netmask))]
    (if (and usable? (not= start end))
      [(int->ip (+ 1 start)) (int->ip end)] ;; exclude network and broadcast
      [(int->ip start) (int->ip (+ 1 end))])))

;; Return a sequence of addresses for an IP and netmask
(defn ip-seq [start end]
  (map int->ip (range (ip->int start) (+ 1 (ip->int end)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field readers and writers

(set! *warn-on-infer* false)

(def readers
  {:buf      #(.slice %1 %2 %3)
   :raw      #(vec (.slice %1 %2 %3))
   :str      #(string/replace (.toString %1 "utf8" %2 %3) "\u0000" "")
   :uint8    #(.readUInt8 %1 %2)
   :uint16   #(.readUInt16BE %1 %2)
   :uint32   #(.readUInt32BE %1 %2)
   :uint64   #(.readBigUInt64BE %1 %2)
   :ipv4     #(vec (.slice %1 %2 (+ 4 %2)))
   :mac      #(vec (.slice %1 %2 (+ 6 %2)))})

(defn arr-fill [dbuf arr off cnt]
  (let [tend (+ off cnt)]
    (.fill dbuf (.from js/Buffer (clj->js arr)) off tend)
    tend))

(defn buf-fill [dbuf sbuf off]
  (let [tend (+ off (.-length sbuf))]
    (.fill dbuf sbuf off tend)
    tend))

;; Returns offset after written value
(def writers
  {:buf      #(buf-fill %1 %2 %3)
   :raw      #(arr-fill %1 %2 %3 (count %2))
   :str      #(do (.write %1 %2 %3 "utf8") (+ (.-length %2) %3))
   :uint8    #(.writeUInt8 %1 %2 %3)
   :uint16   #(.writeUInt16BE %1 %2 %3)
   :uint32   #(.writeUInt32BE %1 %2 %3)
   :uint64   #(.writeBigUInt64BE %1 %2 %3)
   :ipv4     #(arr-fill %1 %2 %3 4)
   :mac      #(arr-fill %1 %2 %3 6)})

