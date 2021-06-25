(ns dhcp.node-client
  (:require [protocol.addrs :as addrs]
            [protocol.socket :as socket]
            [dhcp.core :as dhcp]
            [dhcp.util :as util]))

(def minimist (js/require "minimist"))
(def dgram (js/require "dgram"))
(def pcap (js/require "pcap"))

(defn client-message-handler [cfg msg]
  (let [{:keys [verbose sock if-name hw-addr server-ip state]} cfg
        msg-map (when msg
                  (dhcp/read-dhcp msg))
        msg-type (:opt/msg-type msg-map)
        chaddr (:chaddr msg-map) ]
    (if (and msg-map (not= hw-addr chaddr))
      (when verbose
        (println "Ignoring" msg-type "for"
                 (if chaddr (addrs/octet->mac chaddr) "") "(not us)"))
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
                   "message from" (addrs/octet->ip (:siaddr msg-map))))
        (if (= :ACK msg-type)
          (do
            (util/set-ip-address if-name
                                 (addrs/octet->ip (:yiaddr msg-map))
                                 (addrs/octet->ip (:opt/netmask msg-map)))
            (swap! state assoc :assigned? true ))
          (if resp-msg-map
            (let [buf (dhcp/write-dhcp resp-msg-map)]
              (.send sock buf 0 (.-length buf) dhcp/RECV-PORT server-ip
                     #(if %1
                        (println "Send failed:" %1)
                        (println "Sent" (:opt/msg-type resp-msg-map)
                                 "to" server-ip))))
            (println "Got address:" (addrs/octet->ip (:yiaddr msg-map)))))))))

(defn start-pcap-listener [{:keys [hw-addr if-name] :as cfg}]
  (let [pcap-filter (str "udp and dst port " dhcp/SEND-PORT
                         " and not ether src " (addrs/octet->mac hw-addr))
        psession (.createSession pcap if-name #js {:filter pcap-filter})]
    (println (str "Listening via pcap (filter: '" pcap-filter "')"))
    (doto psession
      (.on "packet" (fn [pkt]
                      (let [buf ^js/Buffer (.-buf pkt)
                            dhcp (.slice buf socket/UDP-PAYLOAD-OFF)]
                        ;;(js/console.log "dhcp buf:" dhcp)
                        (client-message-handler cfg dhcp)))))))

;; TODO: - lease renewal
(defn tick [cfg]
  (let [{:keys [assigned? last-tx-ts]} @(:state cfg)]
    (when (and (not assigned?)
               (> (.getTime (js/Date.)) (+ 5000 last-tx-ts)))
      ;; Trigger DISCOVER
      (client-message-handler cfg nil))))

(defn main [& args]
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
        sock (.createSocket dgram #js {:type "udp4" :reuseAddr true})
        cfg {:if-name if-name
             :hw-addr (addrs/mac->octet hw-addr)
             :server-ip server-ip
             :server-addr (addrs/ip->octet server-ip)
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
