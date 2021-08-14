(ns protocol.addrs
  "Read, write, and manipulate address data.

  Reader functions take `[buf start ctx]` and return `[end value]`
  where `end` is the offset in `buf` after the read value(s).

  Writer functions take `[buf value-or-values start ctx]` and return
  `end` where `end` is the offset in `buf` after the written value(s)."
  (:require [clojure.string :as string]
            [protocol.platform :as plat]
            [protocol.fields :as fields]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network Address support functions

(defn ip->octet "Convert IPv4 string to bytes/octets" [ip]
  (map plat/string->num (string/split ip #"[.]")))

(defn octet->ip "convert bytes/octets to IPv4 string" [os]
  (string/join "." os))

(defn mac->octet "Convert MAC addr string to bytes/octets" [mac]
  (map #(plat/string->num %1 16) (string/split mac #":")))

(defn octet->mac "Convert bytes/octets to MAC addr string" [os]
  (string/join ":" (map fields/int->hex os)))

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
  (let [bin (plat/num->string mask-int 2)]
    (count (filter #(= "1" %) (string/split bin #"")))))

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
  {:ipv4 (fn [b o & as] (let [e (+ 4 o)] [e (octet->ip (plat/buf->vec b o e))]))
   :mac  (fn [b o & as] (let [e (+ 6 o)] [e (octet->mac (plat/buf->vec b o e))]))})

(def ^{:doc "Network address (IPv4 and MAC) writers"}
  writers
  {:ipv4 (fn [b v o & as] (plat/arr-fill b (ip->octet v) o 4))
   :mac  (fn [b v o & as] (plat/arr-fill b (mac->octet v) o 6))})
