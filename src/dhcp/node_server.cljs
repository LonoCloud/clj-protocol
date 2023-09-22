;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.node-server
  "Framework for creating DHCP server implementations."
  (:require [clojure.string :as string]
            [cljs-bean.core :refer [->clj]]
            [protocol.socket :as socket]
            [dhcp.core :as dhcp]
            ["dgram" :as dgram]))

(defn send-message [{:keys [sock log-msg]} msg-map addr port]
  (let [buf (dhcp/write-dhcp msg-map)
        dest (str addr ":" port)]
    (.send sock buf 0 (.-length buf) port addr
           #(if %1
              (log-msg :error (str "Send failed: " %1))
              (log-msg :send msg-map dest)))))

(defn server-message-handler
  "Read/decode DHCP messages from a client, call `message-handler` to
  get a response map, and then write/encode the response and send it
  via `sock`."
  [{:keys [log-msg sock message-handler disable-broadcast] :as cfg} buf rinfo]
  (let [msg-map (dhcp/read-dhcp buf)
        msg-type (:opt/msg-type msg-map)]
    (if (not msg-type)
      (log-msg :error (str "Received invalid msg from " rinfo))
      (let [resp-addr (if (and (not disable-broadcast)
                               (dhcp/MSG-TYPE-BCAST-LOOKUP msg-type))
                        "255.255.255.255"
                        (:address rinfo))
            _ (log-msg :recv msg-map resp-addr)
            resp-msg-map (message-handler cfg msg-map)]
        (if resp-msg-map
          (send-message cfg resp-msg-map resp-addr (:port rinfo))
          (log-msg :error "No msg-map from message-handler, ignoring"))))))


(defn create-server
  "Create a DHCP server listening on `if-name` that will call
  `message-handler` to get a response message for a client message."
  [{:keys [if-name buffsz log-msg
           server-message-handler error-handler] :as cfg
    :or {server-message-handler server-message-handler
         log-msg #(apply println %&)
         error-handler #(prn :err %)}}]
  (let [sock (dgram/createSocket #js {:type "udp4" :reuseAddr true})
        cfg (assoc cfg :sock sock)]
    (doto sock
      (.on "error" error-handler)
      (.on "message"
           (fn [buf rinfo]
             (server-message-handler cfg buf (->clj rinfo))))
      (.on "listening"
           (fn []
             (.setBroadcast sock true)
             (when (not= "all" if-name) (socket/bind-to-device sock if-name))
             (when buffsz (socket/set-rcvbuf sock buffsz))
             (log-msg :info (str "Listening to port " dhcp/RECV-PORT
                                 " on " if-name))))
      (.bind dhcp/RECV-PORT))
    cfg))
