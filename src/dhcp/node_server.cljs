;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.node-server
  "Framework for creating DHCP server implementations. Actual "
  (:require [clojure.string :as string]
            [protocol.socket :as socket]
            [dhcp.core :as dhcp]
            ["dgram" :as dgram]))

(defn server-message-handler
  "Read/decode DHCP messages from a client, call `message-handler` to
  get a response map, and then write/encode the response and send it
  via `sock`."
  [{:keys [sock message-handler disable-bcast log-msg] :as cfg} buf rinfo]
  (let [msg-map (dhcp/read-dhcp buf)
        msg-type (:opt/msg-type msg-map)
        _ (when log-msg
            (log-msg cfg msg-map nil))
        resp-msg-map (message-handler cfg msg-map)
        buf (dhcp/write-dhcp resp-msg-map)
        resp-addr (if (and (not disable-bcast)
                           (dhcp/MSG-TYPE-BCAST-LOOKUP msg-type))
                    "255.255.255.255"
                    (:address rinfo))]
    (.send sock buf 0 (.-length buf) (:port rinfo) resp-addr
           #(if %1
              (println "Send failed:" %1)
              (when log-msg
                (log-msg cfg resp-msg-map (str resp-addr ":" (:port rinfo))))))))

(defn create-server
  "Create a DHCP server listening on `if-name` that will call
  `message-handler` to get a response message for a client message."
  [{:keys [if-name buffsz message-handler] :as cfg}]
  (let [sock (dgram/createSocket #js {:type "udp4" :reuseAddr true})
        cfg (assoc cfg :sock sock)]
    (doto sock
      (.on "error" (fn [& err] (prn :err err)))
      (.on "message" (fn [buf rinfo]
                       (server-message-handler
                         cfg buf (js->clj rinfo :keywordize-keys true))))
      (.on "listening" (fn []
                         (socket/set-reuse-port sock)
                         (.setBroadcast sock true)
                         (when buffsz (socket/set-rcvbuf sock buffsz))
                         (socket/bind-to-device sock if-name)
                         (println "Listening on interface" if-name
                                  "port" dhcp/RECV-PORT)))
      (.bind dhcp/RECV-PORT))))
