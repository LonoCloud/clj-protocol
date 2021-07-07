(ns dhcp.json-pool-server
  (:require [protocol.addrs :as addrs]
            [dhcp.core :as dhcp]
            [dhcp.util :as util]
            [dhcp.node-server :as server]
            [clojure.walk :refer [postwalk]]))

(def ^{:doc "In-memory cache of JSON pool. Format:

            ```
            {:ranges [{:start <START-IP>
                       :end <END-IP>}...]
             :ip-to-mac {<IP> <MAC>...}
             :mac-to-ip {<MAC> <IP>}}
            ```
            "}
  pool (atom nil))

(def ^:private kw-set #{"ranges" "start" "end" "ip-to-mac" "mac-to-ip"})
(defn load-json-pool
  "Read JSON pool data from `leases-file`. See format of [[pool]]."
  [{:keys [leases-file] :as cfg}]
  (let [raw (js->clj (js/JSON.parse (util/slurp leases-file)))
        f (fn [[k v]] (if (kw-set k) [(keyword k) v] [k v]))]
    (postwalk  (fn [x] (if (map? x) (into {} (map f x)) x)) raw)))

(defn save-json-pool
  "Write JSON pool data to `leases-file`"
  [{:keys [leases-file] :as cfg} data]
  (util/spit leases-file (js/JSON.stringify (clj->js data))))

(defn json-pool-init
  "Initialize the in-memory `pool` (see [[pool]] for format) either
  from a `leases-file` if it exists or with an empty pool using
  `if-info` to specify the pool network ranges."
  [{:keys [leases-file pool save-pool load-pool if-info] :as cfg}]
  (let [{:keys [address netmask mac]} if-info]
    (if (util/existsSync leases-file)
      (do
        (println "Loading leases file:" leases-file)
        (reset! pool (load-pool cfg)))
      (let [[start-ip end-ip] (addrs/network-start-end address netmask true)]
        (println "Creating new leases file:" leases-file)
        (reset! pool {:ranges [{:start start-ip :end end-ip}]
                      :ip-to-mac {address mac}
                      :mac-to-ip {mac address}})
        (save-pool cfg @pool)))))

(defn ^:private first-free [ip-to-mac ranges]
  (let [ips (mapcat #(addrs/ip-seq (:start %1) (:end %1)) ranges)]
    (first (filter #(not (contains? ip-to-mac %1)) ips))))

(defn pool-handler
  "Takes a parsed DHCP client message `msg-map` and allocates an
  address from `pool` and responds to the client with that address. If
  the client MAC was already assigned an address then respond with
  the same address."
  [{:keys [pool save-pool if-info] :as cfg} msg-map]
  (let [chaddr (:chaddr msg-map)
        {:keys [ip-to-mac mac-to-ip ranges]} @pool
        cur-ip (get mac-to-ip chaddr)
        ip (or cur-ip (first-free ip-to-mac ranges))]
    (assert ip "DHCP pool exhausted")
    (println (str (and cur-ip "Re-") "Assigning") ip "to" chaddr)
    (swap! pool #(-> %1 (assoc-in [:mac-to-ip %2] %3)
                     (assoc-in [:ip-to-mac %3] %2)) chaddr ip)
    (save-pool cfg @pool)
    (assoc (dhcp/default-response msg-map if-info)
      :yiaddr ip)))

(defn log-message
  "Print a log message for the message in `msg-map`"
  [cfg msg-map addr]
  (let [msg-type (:opt/msg-type msg-map)
        mac (:chaddr msg-map)]
    (if (#{:DISCOVER :REQUEST} msg-type)
      (println "Received" msg-type "for" mac "from" addr)
      (println "Sent" msg-type "for"  mac "to" addr
               "with yiaddr" (:yiaddr msg-map)))))

(defn main
  "Start a JSON pool DHCP server listening on `if-name` and storing
  the JSON lease data in 'dhcp-leases.json'"
  [if-name & args]
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

