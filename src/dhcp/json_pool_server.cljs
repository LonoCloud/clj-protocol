(ns dhcp.json-pool-server
  (:require [dhcp.core :as dhcp]
            [dhcp.node-server :as server]
            [clojure.walk :refer [postwalk]]))

(def fs (js/require "fs"))
(defn slurp [f] (.readFileSync fs f "utf-8"))
(defn spit [f data] (.writeFileSync fs f data))

;; {:ranges [{:start <START-IP>
;;              :end <END-IP>}...]
;;  :ip-to-mac {<IP> <MAC>...}
;;  :mac-to-ip {<MAC> <IP>}}
(def pool (atom nil))

(def kw-set #{"ranges" "start" "end" "ip-to-mac" "mac-to-ip"})
(defn load-pool [path]
  (let [raw (js->clj (js/JSON.parse (slurp path)))
        f (fn [[k v]] (if (kw-set k) [(keyword k) v] [k v]))]
    (postwalk  (fn [x] (if (map? x) (into {} (map f x)) x)) raw)))

(defn save-pool [path data] (spit path (js/JSON.stringify (clj->js data))))

(defn first-free [ip-to-mac ranges]
  (let [ips (mapcat #(dhcp/ip-seq (:start %1) (:end %1)) ranges)]
    (first (filter #(not (contains? ip-to-mac %1)) ips))))

(defn json-pool-init [cfg]
  (let [{:keys [leases-file if-info]} cfg
        {:keys [address netmask]} if-info]
    (if (.existsSync fs leases-file)
      (do
        (println "Loading leases file:" leases-file)
        (reset! pool (load-pool leases-file)))
      (let [[start-ip end-ip] (dhcp/network-start-end address netmask true)]
        (println "Creating new leases file:" leases-file)
        (reset! pool {:ranges [{:start start-ip :end end-ip}]
                      :ip-to-mac {}
                      :mac-to-ip {}})
        (save-pool leases-file @pool)))))

(defn json-pool-handler [cfg msg-map]
  (let [chaddr (dhcp/octet->mac (:chaddr msg-map))
        {:keys [ip-to-mac mac-to-ip ranges]} @pool
        cur-ip (get mac-to-ip chaddr)
        ip (or cur-ip (first-free ip-to-mac ranges))]
    (assert ip "DHCP pool exhausted")
    (println (str (and cur-ip "Re-") "Assigning") ip "to" chaddr)
    (swap! pool #(-> %1 (assoc-in [:mac-to-ip %2] %3)
                     (assoc-in [:ip-to-mac %3] %2)) chaddr ip)
    (save-pool (:leases-file cfg) @pool)
    (assoc (dhcp/default-response msg-map (:octets (:if-info cfg)))
      :yiaddr (dhcp/ip->octet ip))))


(defn -main [if-name & args]
  (when-not if-name
    (println "Must specify an interface name")
    (.exit js/process 0))

  (let [if-info (server/get-if-ipv4 if-name)
        cfg {:message-handler json-pool-handler
             :leases-file "dhcp-leases.json"
             :if-name if-name
             :if-info if-info}]

    (json-pool-init cfg)
    (server/create-server cfg)))

;; Only set main if we are being run
(try
  (when (re-seq #"json_pool_server" (.-id js/module))
    (set! *main-cli-fn* -main))
  (catch :default exc nil))

