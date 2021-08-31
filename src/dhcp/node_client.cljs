;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.node-client
  "A simple DHCP client."
  (:require [protocol.socket :as socket]
            [dhcp.core :as dhcp]
            [dhcp.util :as util]
            ["minimist" :as minimist]
            ["dgram" :as dgram]
            ["pcap" :as pcap]))


(defn client-message-handler
  "Respond to DHCP messages. Read the DHCP message from `buf`, and
  depending on the message and configuration in `cfg` send an
  appropriate response. If `buf` is nil then this means we are
  responding to a trigger (i.e. send DISCOVER) rather than to an
  actual received message."
  [cfg buf]
  (let [{:keys [verbose sock if-name hw-addr server-ip state]} cfg
        msg-map (when buf
                  (dhcp/read-dhcp buf))
        msg-type (:opt/msg-type msg-map)
        chaddr (:chaddr msg-map) ]
    (if (and msg-map (not= hw-addr chaddr))
      (when verbose
        (println "Ignoring" msg-type "for"
                 (if chaddr chaddr "") "(not us)"))
      (let [;; _ (prn :msg-map msg-map)
            resp-defaults {:chaddr hw-addr}
            resp-msg-map (condp = msg-type
                           nil    (merge resp-defaults
                                         {:op 1 :opt/msg-type :DISCOVER})
                           :OFFER (merge resp-defaults
                                         {:op 1 :opt/msg-type :REQUEST})
                           :ACK   nil
                           :NACK (println "Received NACK")
                           (throw (println (str "Received invalid msg type"
                                                msg-type))))]
        (swap! state assoc :last-tx-ts (.getTime (js/Date.)))
        (when msg-map
          (println "Received" msg-type
                   "message from" (:siaddr msg-map)))
        (if (= :ACK msg-type)
          (do
            (util/set-ip-address if-name
                                 (:yiaddr msg-map)
                                 (:opt/netmask msg-map))
            (swap! state assoc :assigned? true ))
          (if resp-msg-map
            (let [buf (dhcp/write-dhcp resp-msg-map)]
              (.send sock buf 0 (.-length buf) dhcp/RECV-PORT server-ip
                     #(if %1
                        (println "Send failed:" %1)
                        (println "Sent" (:opt/msg-type resp-msg-map)
                                 "to" server-ip))))
            (println "Got address:" (:yiaddr msg-map))))))))

(defn start-pcap-listener
  "Start a pcap listener socket so that we can receive broadcast
  messages on an interface `if-name` even before we have an address
  assigned to that interface. We only listen for UDP packets that we
  didn't send (from `hw-addr`) and that are destined for
  [[dhcp/SEND-PORT]]"
  [{:keys [hw-addr if-name] :as cfg}]
  (let [pcap-filter (str "udp and dst port " dhcp/SEND-PORT
                         " and not ether src " hw-addr)
        psession (pcap/createSession if-name #js {:filter pcap-filter})]
    (println (str "Listening via pcap (filter: '" pcap-filter "')"))
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

(defn main
  "Start a DHCP client that will get an address assignment from a DHCP
  server and update the specified interface address."
  [& args]
  (let [minimist-opts {:alias {:v :verbose
                               :i :if-name
                               :s :server-ip
                               :u :unicast}
                       :default {:verbose false
                                 :if-name "eth0"
                                 :server-ip "255.255.255.255"
                                 :unicast false}}
        opts (js->clj (minimist (apply array args) (clj->js minimist-opts))
                      :keywordize-keys true)
        {:keys [verbose if-name server-ip unicast]} opts

        hw-addr (util/get-mac-address if-name)
        sock (dgram/createSocket #js {:type "udp4" :reuseAddr true})
        cfg {:if-name if-name
             :hw-addr hw-addr
             :server-ip server-ip
             :sock sock
             :state (atom {:assigned? false
                           :last-tx-ts 0})}]
    (println (str "Binding to " if-name " (" hw-addr ")"))
    ;; To listen before we have an address, we need pcap
    (doto sock
      (.on "listening" (fn []
                         (println "Listening on port" dhcp/SEND-PORT)
                         (socket/bind-to-device sock if-name)
                         #_(socket/freebind sock)
                         (.setBroadcast sock true)))
      (.bind dhcp/SEND-PORT server-ip))
    (if unicast
      (doto sock
        (.on "message" (fn [msg rinfo]
                         (client-message-handler cfg msg))))
      (start-pcap-listener cfg))
    (js/setInterval #(tick cfg) 1000)))
