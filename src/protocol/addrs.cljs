(ns protocol.addrs
  "Read, write, and manipulate address data.

  Reader functions take `[buf start ctx]` and return `[end value]`
  where `end` is the offset in `buf` after the read value(s).

  Writer functions take `[buf value-or-values start ctx]` and return
  `end` where `end` is the offset in `buf` after the written value(s)."
  (:require [protocol.fields :as fields]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network Address support functions

(defn ip->octet "Convert IPv4 string to bytes/octets" [ip]
  (map int (.split ip ".")))

(defn octet->ip "convert bytes/octets to IPv4 string" [os]
  (.join (clj->js os) "."))

(defn mac->octet "Convert MAC addr string to bytes/octets" [mac]
  (map #(js/parseInt %1 16) (.split mac ":")))

(defn octet->mac "Convert bytes/octets to MAC addr string" [os]
  (.join (clj->js (map fields/int->hex os)) ":"))

(defn ip->int "Convert IPv4 string to uint32 value" [ip]
  (fields/octet->int (ip->octet ip)))

(defn int->ip "Convert IPv4 uint32 value to IPv4 string" [num]
  (octet->ip (fields/int->octet num 4)))

(defn first-ip
  "Return first IPv4 addr based on `ip` and `netmask`"
  [ip netmask]
  (octet->ip
   (map #(bit-and %1 %2) (ip->octet ip) (ip->octet netmask))))

(defn broadcast
  "Return last/broadcast IPv4 addr based on `ip` and `netmask`"
  [ip netmask]
  (octet->ip
   (map #(bit-or %1 %2)
        (ip->octet ip)
        (map #(+ 256 (bit-not %1)) (ip->octet netmask)))))

(defn mask-int->prefix
  "Convert network mask as uint32 IPv4 value to CIDR prefix value"
  [mask-int]
  (let [bin (.toString mask-int 2)]
    (count (filter #(= "1" %) (.split bin "")))))

(defn mask-ip->prefix
  "Convert network mask as IP string to CIDR prefix value"
  [mask-ip]
  (mask-int->prefix (ip->int mask-ip)))

(defn network-start-end
  "Return pair of first and last IPv4 address based on `ip` and
  `netmask`. If `usable?` is true then omit the network and broadcast
  addresses."
  [ip netmask & [usable?]]
  (let [start (ip->int (first-ip ip netmask))
        end (ip->int (broadcast ip netmask))]
    (if (and usable? (not= start end))
      [(int->ip (+ 1 start)) (int->ip (- end 1))] ;; exclude network and broadcast
      [(int->ip start) (int->ip end)])))

(defn ip-seq
  "Return a sequence of IPv4 addresses between `start` and `end`"
  [start end]
  (map int->ip (range (ip->int start) (+ 1 (ip->int end)))))

;;;

(def ^{:doc "Network address (IPv4 and MAC) readers"}
  readers
  {:ipv4 #(let [e (+ 4 %2)] [e (octet->ip (fields/buf->vec %1 %2 e))])
   :mac  #(let [e (+ 6 %2)] [e (octet->mac (fields/buf->vec %1 %2 (+ 6 %2)))])})

(def ^{:doc "Network address (IPv4 and MAC) writers"}
  writers
  {:ipv4 #(fields/arr-fill %1 (ip->octet %2) %3 4)
   :mac  #(fields/arr-fill %1 (mac->octet %2) %3 6)})
