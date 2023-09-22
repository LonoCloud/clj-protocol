;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.node-client
  "Framework for creating DHCP client implementations. Also includes
  a runnable DHCP client."
  (:require [protocol.socket :as socket]
            [dhcp.core :as dhcp]
            [dhcp.util :as util]
            ["dgram" :as dgram]
            ["pcap" :as pcap]))

(defn send-message [{:keys [sock log-msg]} msg-map addr port]
  (let [buf (dhcp/write-dhcp msg-map)
        dest (str addr ":" port)]
    (.send sock buf 0 (.-length buf) port addr
           #(if %1
              (log-msg :error (str "Send failed: " %1))
              (log-msg :send msg-map dest)))))

(defn client-message-handler
  "Read/decode DHCP messages from a server. If `buf` is nil then this
  means we are responding to a trigger (i.e. send DISCOVER) rather
  than to an actual received message."
  [cfg buf rinfo]
  (let [{:keys [log-msg sock message-handler if-name hw-addr server-ip]} cfg
        msg-map (if buf
                  (dhcp/read-dhcp buf)
                  {:opt/msg-type :start :chaddr hw-addr :siaddr "SELF"})
        msg-type (:opt/msg-type msg-map)
        chaddr (:chaddr msg-map) ]
    (if (and buf (not= hw-addr chaddr))
      (log-msg :info (str "Ignoring " msg-type " for "
               (if chaddr chaddr "") " (not us)"))
      (let [_ (log-msg :recv msg-map (:siaddr msg-map))
            resp-msg-map (message-handler cfg msg-map)]
        (if resp-msg-map
          (send-message cfg resp-msg-map server-ip dhcp/RECV-PORT))))))

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
                        (client-message-handler cfg dhcp nil)))))))

(defn create-client
  [{:keys [if-name buffsz server-ip unicast
           client-message-handler log-msg error-handler] :as cfg
    :or {client-message-handler client-message-handler
         log-msg #(apply println %&)
         error-handler #(prn :err %)}}]
  (let [sock (dgram/createSocket #js {:type "udp4" :reuseAddr true})
        hw-addr (util/get-mac-address if-name)
        cfg (merge cfg
                  {:sock sock
                   :hw-addr hw-addr})]
    (log-msg :info (str "Binding to " if-name " (" hw-addr ")"))
    ;; To listen before we have an address, we need pcap
    (doto sock
      (.on "error" error-handler)
      (.on "listening"
           (fn []
             (when buffsz (socket/set-rcvbuf sock buffsz))
             (socket/bind-to-device sock if-name)
             (.setBroadcast sock true)
             (log-msg :info (str "Listening to port "
                                 dhcp/SEND-PORT " on " if-name))))
      (.bind dhcp/SEND-PORT (when-not unicast "255.255.255.255")))
    (if unicast
      (doto sock
        (.on "message" (fn [buf rinfo]
                         (client-message-handler cfg buf rinfo))))
      (start-pcap-listener cfg))
    cfg))


