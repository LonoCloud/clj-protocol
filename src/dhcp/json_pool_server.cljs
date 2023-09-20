;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.json-pool-server
  (:require [protocol.addrs :as addrs]
            [dhcp.core :as dhcp]
            [dhcp.util :as util]
            [dhcp.logging :as logging]
            [dhcp.node-server :as server]
            [clojure.edn :as edn]
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
  `server-info` to specify the pool network ranges."
  [{:keys [log-msg leases-file pool save-pool load-pool server-info] :as cfg}]
  (let [{:keys [address netmask mac]} server-info]
    (if (util/existsSync leases-file)
      (do
        (log-msg :info (str "Loading leases file: " leases-file))
        (reset! pool (load-pool cfg)))
      (let [[start-ip end-ip] (addrs/network-start-end address netmask true)]
        (log-msg :info (str "Creating new leases file: " leases-file))
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
  [{:keys [log-msg pool save-pool server-info] :as cfg} msg-map]
  (let [field-overrides (:fields cfg) ;; config file field/option overrides
        chaddr (:chaddr msg-map)
        {:keys [ip-to-mac mac-to-ip ranges]} @pool
        cur-ip (get mac-to-ip chaddr)
        ip (or cur-ip (first-free ip-to-mac ranges))]
    (if (not ip)
      (log-msg :error "DHCP pool exhausted")
      (do
        (log-msg :info (str (if cur-ip "Re-") "Assigning " ip " to " chaddr))
        (swap! pool #(-> %1 (assoc-in [:mac-to-ip %2] %3)
                         (assoc-in [:ip-to-mac %3] %2)) chaddr ip)
        (save-pool cfg @pool)
        (merge
          (dhcp/default-response msg-map server-info)
          (select-keys msg-map [:giaddr :opt/relay-agent-info])
          {:yiaddr ip}
          field-overrides)))))

(defn main
  "Start a JSON pool DHCP server listening on `if-name` and storing
  the JSON lease data in 'dhcp-leases.json'"
  [if-name & [config-file args]]

  (when-not if-name
    (util/fatal 2 "Must specify an interface name"))

  (let [if-info (util/get-if-ipv4 if-name)
        file-cfg (when config-file
                   (edn/read-string (util/slurp config-file)))
        log-msg logging/log-message

        ;; precedence: CLI opts, file config, discovered interface info
        user-cfg (util/deep-merge {:leases-file "dhcp-leases.json"
                                   :if-name if-name
                                   :server-info if-info
                                   :log-level 2}
                                  file-cfg)
        cfg (merge
              user-cfg
              {:message-handler pool-handler
               :log-msg logging/log-message
               :pool pool
               :load-pool load-json-pool
               :save-pool save-json-pool})]

    (logging/start-logging cfg)
    (log-msg :info (str "User config: " user-cfg))

    (json-pool-init cfg)

    (log-msg :info "Starting DHCP Server...")
    (server/create-server cfg)))

