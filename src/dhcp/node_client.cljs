;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.node-client
  "A simple DHCP client."
  (:require [protocol.socket :as socket]
            [dhcp.core :as dhcp]
            [dhcp.util :as util]
            [dhcp.logging :as logging]
            ["dgram" :as dgram]
            ["pcap" :as pcap]))

(def minimist (js/require "minimist"))

(def USAGE "Usage: dhcp-client [options]
Options:
  --if-name|-i INTF  - Bind to INTF [default: eth0]
  --server-ip|-s IP  - Send to IP [default: 255.255.255.255]
  --unicast          - Listen for unicast responses instead of broadcast
  --log-level LEVEL  - Set logging level to LEVEL [default: 2]
                         0 - none
                         1 - one character per event
                         2 - full log line for each event
")

(defn client-message-handler
  "Respond to DHCP messages. Read the DHCP message from `buf`, and
  depending on the message and configuration in `cfg` send an
  appropriate response. If `buf` is nil then this means we are
  responding to a trigger (i.e. send DISCOVER) rather than to an
  actual received message."
  [cfg buf]
  (let [{:keys [log-msg sock if-name hw-addr server-ip state]} cfg
        msg-map (if buf
                  (dhcp/read-dhcp buf)
                  {:opt/msg-type :start :chaddr hw-addr :siaddr "SELF"})
        msg-type (:opt/msg-type msg-map)
        chaddr (:chaddr msg-map) ]
    (if (and buf (not= hw-addr chaddr))
      (log-msg :info "Ignoring" msg-type "for"
               (if chaddr chaddr "") "(not us)")
      (let [;; _ (prn :msg-map msg-map)
            resp-defaults {:chaddr hw-addr}
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
        (log-msg :recv msg-map (:siaddr msg-map))
        (if (= :ACK msg-type)
          (do
            (log-msg :info (str "Setting " if-name " to " (:yiaddr msg-map)
                                " (" (:opt/netmask msg-map) ")"))
            (util/set-ip-address if-name
                                 (:yiaddr msg-map)
                                 (:opt/netmask msg-map))
            (swap! state assoc :assigned? true ))
          (if resp-msg-map
            (let [buf (dhcp/write-dhcp resp-msg-map)]
              (.send sock buf 0 (.-length buf) dhcp/RECV-PORT server-ip
                     #(if %1
                        (log-msg :error (str "Send failed: " %1))
                        (log-msg :send resp-msg-map server-ip))))
            (log-msg :info (str "Got address: " (:yiaddr msg-map)))))))))

(defn start-pcap-listener
  "Start a pcap listener socket so that we can receive broadcast
  messages on an interface `if-name` even before we have an address
  assigned to that interface. We only listen for UDP packets that we
  didn't send (from `hw-addr`) and that are destined for
  [[dhcp/SEND-PORT]]"
  [{:keys [log-msg hw-addr if-name] :as cfg}]
  (let [pcap-filter (str "udp and dst port " dhcp/SEND-PORT
                         " and not ether src " hw-addr)
        psession (pcap/createSession if-name #js {:filter pcap-filter})]
    (log-msg :info (str "Listening via pcap (filter: '" pcap-filter "')"))
    (doto psession
      (.on "packet" (fn [pkt]
                      (let [buf ^js/Buffer (.-buf pkt)
                            dhcp (.slice buf socket/UDP-PAYLOAD-OFF)]
                        ;;(js/console.log "dhcp buf:" dhcp)
                        (client-message-handler cfg dhcp)))))))

;; TODO: - lease renewal
(defn tick
  "Periodically trigger DISCOVER messages until we are assigned an
  address."
  [cfg]
  (let [{:keys [assigned? last-tx-ts]} @(:state cfg)]
    (when (and (not assigned?)
               (> (.getTime (js/Date.)) (+ 5000 last-tx-ts)))
      ;; Trigger DISCOVER
      (client-message-handler cfg nil))))

(defn create-client [{:keys [log-msg if-name server-ip unicast] :as cfg
                      :or {log-msg #(apply println %&)}}]
  (let [hw-addr (util/get-mac-address if-name)
        sock (dgram/createSocket #js {:type "udp4" :reuseAddr true})
        hw-addr (util/get-mac-address if-name)
        cfg (merge cfg
                  {:sock sock
                   :hw-addr hw-addr
                   :state (atom {:assigned? false
                                 :last-tx-ts 0})})]
    (log-msg :info (str "Binding to " if-name " (" hw-addr ")"))
    ;; To listen before we have an address, we need pcap
    (doto sock
      (.on "listening" (fn []
                         (log-msg :info (str "Listening on port "
                                             dhcp/SEND-PORT))
                         (socket/bind-to-device sock if-name)
                         (.setBroadcast sock true)))
      (.bind dhcp/SEND-PORT (when-not unicast "255.255.255.255")))
    (if unicast
      (doto sock
        (.on "message" (fn [msg rinfo]
                         (client-message-handler cfg msg))))
      (start-pcap-listener cfg))
    (js/setInterval #(tick cfg) 1000)
    cfg))

(defn main
  "Start a DHCP client that will get an address assignment from a DHCP
  server and update the specified interface address."
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
        cfg (assoc (dissoc opts :_) :log-msg log-msg)]

    (logging/start-logging cfg)
    (log-msg :info (str "User config: " cfg))

    (log-msg :info "Starting DHCP Client...")
    (create-client cfg)))
