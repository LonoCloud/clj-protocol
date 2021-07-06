(ns protocol.addrs
  (:require [protocol.fields :as fields]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network Address support functions

(defn ip->octet [s]
  (map int (.split s ".")))

(defn octet->ip [o]
  (.join (clj->js o) "."))

(defn mac->octet [s]
  (map #(js/parseInt %1 16) (.split s ":")))

(defn octet->mac [o]
  (.join (clj->js (map fields/int->hex o)) ":"))

(defn ip->int [ip]
  (fields/octet->int (ip->octet ip)))

(defn int->ip [int]
  (octet->ip (fields/int->octet int 4)))

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
      [(int->ip (+ 1 start)) (int->ip (- end 1))] ;; exclude network and broadcast
      [(int->ip start) (int->ip end)])))

(defn ip-seq
  "Return a sequence of addresses for an IP and netmask"
  [start end]
  (map int->ip (range (ip->int start) (+ 1 (ip->int end)))))

;;;

(def readers
  {:ipv4      #(let [e (+ 4 %2)] [e (octet->ip (.slice %1 %2 e))])
   :mac       #(let [e (+ 6 %2)] [e (octet->mac (.slice %1 %2 (+ 6 %2)))])})

(def writers
  {:ipv4      #(fields/arr-fill %1 (ip->octet %2) %3 4)
   :mac       #(fields/arr-fill %1 (mac->octet %2) %3 6)})
