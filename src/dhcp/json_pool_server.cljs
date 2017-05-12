(ns dhcp.json-pool-server
  (:require [dhcp.core :as dhcp]
            [dhcp.node-server :as server]
            [clojure.walk :refer [postwalk]]))

;; {:ranges [{:start <START-IP>
;;            :end <END-IP>}...]
;;  :ip-to-mac {<IP> <MAC>...}
;;  :mac-to-ip {<MAC> <IP>}}
(def pool (atom nil))

(def fs (js/require "fs"))
(defn slurp [f] (.readFileSync fs f "utf-8"))
(defn spit [f data] (.writeFileSync fs f data))

(def kw-set #{"ranges" "start" "end" "ip-to-mac" "mac-to-ip"})
(defn load-json-pool [cfg]
  (let [raw (js->clj (js/JSON.parse (slurp (:leases-file cfg))))
        f (fn [[k v]] (if (kw-set k) [(keyword k) v] [k v]))]
    (postwalk  (fn [x] (if (map? x) (into {} (map f x)) x)) raw)))

(defn save-json-pool [cfg data]
  (spit (:leases-file cfg) (js/JSON.stringify (clj->js data))))

(defn json-pool-init [cfg]
  (let [{:keys [leases-file pool save-pool load-pool if-info]} cfg
        {:keys [address netmask]} if-info]
    (if (.existsSync fs leases-file)
      (do
        (println "Loading leases file:" leases-file)
        (reset! pool (load-pool cfg)))
      (let [[start-ip end-ip] (dhcp/network-start-end address netmask true)]
        (println "Creating new leases file:" leases-file)
        (reset! pool {:ranges [{:start start-ip :end end-ip}]
                      :ip-to-mac {}
                      :mac-to-ip {}})
        (save-pool cfg @pool)))))

(defn -main [if-name & args]
  (when-not if-name
    (println "Must specify an interface name")
    (.exit js/process 0))

  (let [if-info (server/get-if-ipv4 if-name)
        cfg {:message-handler server/pool-handler
             :leases-file "dhcp-leases.json"
             :pool pool
             :load-pool load-json-pool
             :save-pool save-json-pool
             :if-name if-name
             :if-info if-info}]

    (json-pool-init cfg)
    (server/create-server cfg)))

;; Only set main if we are being run
;;(try
;;  (when (re-seq #"json_pool_server" (.-id js/module))
;;    (set! *main-cli-fn* -main))
;;  (catch :default exc nil))

