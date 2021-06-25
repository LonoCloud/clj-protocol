(ns dhcp.json-pool-server
  (:require [protocol.addrs :as addrs]
            [dhcp.core :as dhcp]
            [dhcp.util :as util]
            [dhcp.node-server :as server]
            [clojure.walk :refer [postwalk]]))

(def fs (js/require "fs"))
(defn slurp [f] (.readFileSync fs f "utf-8"))
(defn spit [f data] (.writeFileSync fs f data))

;; {:ranges [{:start <START-IP>
;;            :end <END-IP>}...]
;;  :ip-to-mac {<IP> <MAC>...}
;;  :mac-to-ip {<MAC> <IP>}}
(def pool (atom nil))

(def kw-set #{"ranges" "start" "end" "ip-to-mac" "mac-to-ip"})
(defn load-json-pool [cfg]
  (let [raw (js->clj (js/JSON.parse (slurp (:leases-file cfg))))
        f (fn [[k v]] (if (kw-set k) [(keyword k) v] [k v]))]
    (postwalk  (fn [x] (if (map? x) (into {} (map f x)) x)) raw)))

(defn save-json-pool [cfg data]
  (spit (:leases-file cfg) (js/JSON.stringify (clj->js data))))

(defn json-pool-init [cfg]
  (let [{:keys [leases-file pool save-pool load-pool if-info]} cfg
        {:keys [address netmask mac]} if-info]
    (if (.existsSync fs leases-file)
      (do
        (println "Loading leases file:" leases-file)
        (reset! pool (load-pool cfg)))
      (let [[start-ip end-ip] (addrs/network-start-end address netmask true)]
        (println "Creating new leases file:" leases-file)
        (reset! pool {:ranges [{:start start-ip :end end-ip}]
                      :ip-to-mac {address mac}
                      :mac-to-ip {mac address}})
        (save-pool cfg @pool)))))

(defn first-free [ip-to-mac ranges]
  (let [ips (mapcat #(addrs/ip-seq (:start %1) (:end %1)) ranges)]
    (first (filter #(not (contains? ip-to-mac %1)) ips))))

;; (:pool cfg) should be an atom containing:
;;     {:ranges [{:start <START-IP>
;;                :end <END-IP>}...]
;;      :ip-to-mac {<IP> <MAC>...}
;;      :mac-to-ip {<MAC> <IP>}}
(defn pool-handler [cfg msg-map]
  (let [{:keys [pool save-pool if-info]} cfg
        chaddr (addrs/octet->mac (:chaddr msg-map))
        {:keys [ip-to-mac mac-to-ip ranges]} @pool
        cur-ip (get mac-to-ip chaddr)
        ip (or cur-ip (first-free ip-to-mac ranges))]
    (assert ip "DHCP pool exhausted")
    (println (str (and cur-ip "Re-") "Assigning") ip "to" chaddr)
    (swap! pool #(-> %1 (assoc-in [:mac-to-ip %2] %3)
                     (assoc-in [:ip-to-mac %3] %2)) chaddr ip)
    (save-pool cfg @pool)
    (assoc (dhcp/default-response msg-map (:octets if-info))
      :yiaddr (addrs/ip->octet ip))))

(defn log-message [cfg msg-map addr]
  (let [msg-type (:opt/msg-type msg-map)
        mac (addrs/octet->mac (:chaddr msg-map))]
    (if (#{:DISCOVER :REQUEST} msg-type)
      (println "Received" msg-type "for" mac "from" addr)
      (println "Sent" msg-type "for"  mac "to" addr
               "with yiaddr" (addrs/octet->ip (:yiaddr msg-map))))))

(defn log-lease [cfg msg-map cur-ip ip chaddr]
  (println (str (and cur-ip "Re-") "Assigning") ip "to" chaddr))


(defn main [if-name & args]
  (when-not if-name
    (println "Must specify an interface name")
    (.exit js/process 0))

  (let [if-info (util/get-if-ipv4 if-name)
        cfg {:message-handler pool-handler
             :leases-file "dhcp-leases.json"
             :log-msg log-message
             :pool pool
             :load-pool load-json-pool
             :save-pool save-json-pool
             :if-name if-name
             :if-info if-info}]

    (json-pool-init cfg)
    (server/create-server cfg)))

;; Only set main if we are being run
;;(try
;;  (when (re-seq #"realm_server" (.-id js/module))
;;    (set! *main-cli-fn* -main))
;;  (catch :default exc nil))

