(ns dhcp.node-server
  (:require [clojure.string :as string]
            [protocol.socket :as socket]
            [dhcp.core :as dhcp]))

(def dgram (js/require "dgram"))

(defn server-message-handler [cfg msg rinfo]
  (let [{:keys [sock if-info disable-bcast]} cfg
        msg-map (dhcp/read-dhcp msg)
        msg-type (:opt/msg-type msg-map)
        _ (when-let [logfn (:log-msg cfg)]
            (logfn cfg msg-map nil))
        resp-msg-map ((:message-handler cfg) cfg msg-map)
        buf (dhcp/write-dhcp resp-msg-map)
        resp-addr (if (and (not disable-bcast)
                           (dhcp/MSG-TYPE-BCAST-LOOKUP msg-type))
                    "255.255.255.255"
                    (:address rinfo))]
    (.send sock buf 0 (.-length buf) (:port rinfo) resp-addr
           #(if %1
              (println "Send failed:" %1)
              (when-let [logfn (:log-msg cfg)]
                (logfn cfg resp-msg-map (str resp-addr ":" (:port rinfo))))))))

(defn create-server [cfg]
  (let [{:keys [if-name buffsz]} cfg
        sock (.createSocket dgram #js {:type "udp4" :reuseAddr true})
        cfg (assoc cfg :sock sock)]
    (doto sock
      (.on "error" (fn [& err] (prn :err err)))
      (.on "message" (fn [msg rinfo]
                       (server-message-handler
                        cfg msg (js->clj rinfo :keywordize-keys true))))
      (.on "listening" (fn []
                         (socket/set-reuse-port sock)
                         (.setBroadcast sock true)
                         (when buffsz (socket/set-rcvbuf sock buffsz))
                         (socket/bind-to-device sock if-name)
                         (println "Listening on interface" if-name
                                  "port" dhcp/RECV-PORT)))
      (.bind dhcp/RECV-PORT))))
