;; Copyright (c) 2023, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.simple-client
  (:require [dhcp.util :as util]
            [dhcp.logging :as logging]
            [dhcp.node-client :as client]))

(def minimist (js/require "minimist"))

(def USAGE "Usage: simple-client [options]
Options:
  --if-name|-i INTF  - Bind to INTF [default: eth0]
  --server-ip|-s IP  - Send to IP [default: 255.255.255.255]
  --unicast          - Listen for unicast responses instead of broadcast
  --log-level LEVEL  - Set logging level to LEVEL [default: 2]
                         0 - none
                         1 - one character per event
                         2 - full log line for each event
")

(defn simple-client-message-handler
  [cfg msg-map]
  (let [{:keys [log-msg if-name hw-addr state]} cfg
        resp-defaults {:chaddr hw-addr}
        msg-type (:opt/msg-type msg-map)
        resp-msg-map (condp = msg-type
                       :start (merge resp-defaults
                                     {:op 1 :opt/msg-type :DISCOVER})
                       :OFFER (merge resp-defaults
                                     {:op 1 :opt/msg-type :REQUEST})
                       :ACK   nil
                       :NACK (log-msg :error "Received NACK")
                       (log-msg :error (str "Received invalid msg type "
                                            msg-type)))]
    (swap! state assoc :last-tx-ts (.getTime (js/Date.)))
    (if (= :ACK msg-type)
      (do
        (log-msg :info (str "Setting " if-name " to " (:yiaddr msg-map)
                            " (" (:opt/netmask msg-map) ")"))
        (util/set-ip-address if-name
                             (:yiaddr msg-map)
                             (:opt/netmask msg-map))
        (swap! state assoc :assigned? true )
        ;; Done, no response back
        nil)
      resp-msg-map)))

;; TODO: - lease renewal
(defn tick
  "Periodically trigger DISCOVER messages until we are assigned an
  address."
  [cfg]
  (let [{:keys [assigned? last-tx-ts]} @(:state cfg)]
    (when (and (not assigned?)
               (> (.getTime (js/Date.)) (+ 5000 last-tx-ts)))
      ;; Trigger DISCOVER
      (client/client-message-handler cfg nil nil))))

(defn main
  "Start a simple DHCP client that will get an address assignment from
  a DHCP server and update the specified interface address."
  [& args]
  (let [minimist-opts {:alias {:i :if-name
                               :s :server-ip
                               :u :unicast}
                       :default {:if-name "eth0"
                                 :server-ip "255.255.255.255"
                                 :unicast false
                                 :log-level 2}}
        opts (js->clj (minimist (apply array args) (clj->js minimist-opts))
                      :keywordize-keys true)
        {:keys [h help if-name server-ip unicast]} opts
        _ (when (or h help) (util/fatal 2 USAGE))
        log-msg logging/log-message
        cfg (merge (dissoc opts :_)
                   {:log-msg log-msg
                    :message-handler simple-client-message-handler
                    :state (atom {:assigned? false
                                  :last-tx-ts 0})})]

    (logging/start-logging cfg)
    (log-msg :info (str "User config: " opts))

    (log-msg :info "Starting DHCP Client...")
    (let [cfg (client/create-client cfg)]
      (js/setInterval #(tick cfg) 1000))))
