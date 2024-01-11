;; Copyright (c) 2023, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.mac2ip-server
  "Multiprocess DHCP server that does direct mapping of MAC to IP addresses."
  (:require [cljs-bean.core :refer [->clj ->js]]
            [protocol.fields :as fields]
            [protocol.addrs :as addrs]
            [dhcp.core :as dhcp]
            [dhcp.util :as util]
            [dhcp.logging :as logging]
            [dhcp.node-server :as server]
            [clojure.walk :refer [postwalk]]))

(def cluster (js/require "node:cluster"))
(def minimist (js/require "minimist"))

(def USAGE "Usage: mac2ip-server [options]
Options:
  --processes CNT    - Number of worker processes
                       [default: 1]
  --config-file CFG  - Load json/edn config from CFG
                       [default: mac2ip.json]
  --if-name IF-NAME  - Bind to interface IF-NAME
                       [default: all]
  --log-level LEVEL  - Set logging level to LEVEL:
                       [default: 1]
                         0 - none
                         1 - one character per event
                         2 - full log line for each event
")

(def arg-defaults {:processes 1
                   :if-name "all"
                   :config-file "mac2ip.json"
                   :log-level 1})

(defn load-config
  [config-file]
  (let [cfg (util/load-config config-file)
        ranges (for [r (:ranges cfg)]
                 {:ip-start  (-> r :ip-start  addrs/ip->int)
                  :ip-end    (-> r :ip-end    addrs/ip->int)
                  :mac-start (-> r :mac-start addrs/mac->int)
                  :mac-end   (-> r :mac-end   addrs/mac->int)})]
    (assoc cfg :ranges ranges)))

(defn mac->ip [chaddr ranges]
  (let [mac-int (addrs/mac->int chaddr)
        r (first (filter #(and (>= mac-int (:mac-start %))
                               (<= mac-int (:mac-end %)))
                         ranges))]
    (when r
      (addrs/int->ip (+ (:ip-start r) (- mac-int (:mac-start r)))))))

(defn ip->mac [ip ranges]
  (let [ip-int (addrs/ip->int ip)
        r (first (filter #(and (>= ip-int (:ip-start %))
                               (<= ip-int (:ip-end %)))
                         ranges))]
    (when r
      (addrs/int->mac (+ (:mac-start r) (- ip-int (:ip-start r)))))))

(defn mac2ip-handler [{:keys [ranges server-info log-msg log-level] :as cfg}
                      {:keys [chaddr ciaddr] :as msg-map}]
  (if (= :LEASEQUERY (:opt/msg-type msg-map))
    (let [[mac ip kind] (cond
                          ciaddr [(ip->mac ciaddr ranges) ciaddr "IP"]
                          chaddr [chaddr (mac->ip chaddr ranges) "MAC"]
                          :else  [nil nil "Client-identifier"])
          msg-resp (if (and mac ip)
                     (merge
                       (select-keys msg-map [:giaddr :opt/relay-agent-info])
                       {:opt/msg-type :LEASEACTIVE
                        :chaddr mac
                        :ciaddr ip})
                     {:opt/msg-type :LEASEUNKNOWN})]
      (do
        (condp = log-level
          2 (log-msg :info (str "Leasequery by " kind " "
                                (condp = kind
                                  "IP"  (str ciaddr " to " mac)
                                  "MAC" (str chaddr " to " ip)
                                  :else (str "unsupported"))))
          nil)
        (merge (dhcp/default-response msg-map server-info)
               (:fields cfg) ;; config file field/option overrides
               msg-resp)))

    (let [ip (mac->ip chaddr ranges)]
      (if (not ip)
        (do
          (log-msg :error (str "MAC " chaddr " is out of range"))
          nil)
        (do
          (condp = log-level
            2 (log-msg :info (str "Assigning " ip " to " chaddr))
            nil)
          (merge (dhcp/default-response msg-map server-info)
                 (select-keys msg-map [:giaddr :opt/relay-agent-info])
                 (:fields cfg) ;; config file field/option overrides
                 {:yiaddr ip}))))))

(defn worker [user-cfg]
  (let [log-msg logging/log-message
        cfg (merge
              user-cfg
              {:message-handler mac2ip-handler
               :error-handler #(util/fatal 1 "Could not create server:" %)
               :log-msg log-msg})]

    (logging/start-logging cfg)
    (log-msg :info "Starting DHCP Server...")
    (server/create-server cfg)))

(defn parse-args
  [& args]
  (let [minimist-opts {:default arg-defaults }
        opts (->clj (minimist (apply array args) (->js minimist-opts)))
        {:keys [h help if-name config-file log-level]} opts
        _ (when (or h help) (util/fatal 2 USAGE))
        _ (when-not (util/existsSync config-file)
            (util/fatal 2 "Config file" config-file "does not exist"))
        file-cfg (load-config config-file)
        _ (when (and (= "all" if-name)
                     (not (:server-info file-cfg)))
            (util/fatal 2 "--if-name or config server-info required"))
        if-info (util/get-if-ipv4 if-name)
        _ (when (and (not= "all" if-name)
                     (not if-info))
            (util/fatal 2 "Interface" if-name "not found"))
        ;; precedence: CLI opts, file config, discovered interface info
        user-cfg (util/deep-merge {:server-info if-info
                                   :buffsz (* 16 1024 1024)}
                                  file-cfg
                                  (dissoc opts :_))]

    (when (and (= "all" if-name)
               (not (:disable-broadcast user-cfg)))
      (util/fatal 2 "--if-name or --disable-broadcast must be specified"))

    user-cfg))

(defn main
  "Start mac2ip DHCP server worker processes listening on `if-name`
  using `config-file`"
  [& args]
  (if cluster.isPrimary
    (let [{:keys [processes log-level] :as cfg} (apply parse-args args)]
      (when (= 2 log-level) (println "User config:" cfg))
      (println "Forking" processes "workers")
      (doseq [i (range processes)
              :let [cfg (assoc cfg :log-prefix (str "worker-" i))
                    worker (cluster.fork)]]
        ^object
        (.on worker "message"
             #(let [msg (->clj %)]
                (condp = (:type msg)
                  "ready" (.send worker (->js {:type "start"
                                               :cfg cfg})))))))
    (do
      (.on js/process "message"
           #(let [msg (->clj %)]
              (if (= "start" (:type msg))
                (worker (:cfg msg)))))
      (.send js/process (->js {:type "ready"
                               :pid js/process.pid})))))
