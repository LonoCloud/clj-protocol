(ns dhcp.node-server
  (:require [cljs.nodejs :as nodejs]
            [clojure.string :as string]
            [dhcp.core :as dhcp]
            [dhcp.socket :as socket]))

(nodejs/enable-util-print!)

(def dgram (js/require "dgram"))
(def os (js/require "os"))

(defn get-if-ipv4 [if-name]
  (let [srv-ifs (-> os .networkInterfaces (js->clj :keywordize-keys true))
        srv-if-ipv4 (-> srv-ifs (get (keyword if-name)) first)
        {:keys [address netmask]} srv-if-ipv4]
    (assoc srv-if-ipv4
      :octets {:address (dhcp/ip->octet address)
               :netmask (dhcp/ip->octet netmask)
               :broadcast (dhcp/ip->octet (dhcp/broadcast address netmask))
               :mac (dhcp/mac->octet (:mac srv-if-ipv4))})))

(defn generic-message-handler [cfg msg]
  (let [{:keys [sock if-info]} cfg
        msg-map (dhcp/read-message (-> msg .toJSON .-data js->clj))
        _ (println "Received" (:opt/msg-type msg-map)
                   "message from" (:chaddr msg-map))
        resp-msg-map ((:message-handler cfg) cfg msg-map)
        buf (js/Buffer. (clj->js (dhcp/write-message resp-msg-map)))
        resp-addr (string/join "." (:broadcast (:octets if-info)))]
    (.send sock buf 0 (.-length buf) dhcp/SEND-PORT resp-addr
           #(if %1
              (println "Send failed:" %1)
              (println "Sent" (:opt/msg-type resp-msg-map)
                       "message with" (:yiaddr resp-msg-map)
                       "to" (:chaddr resp-msg-map))))))

(defn create-server [cfg]
  (let [{:keys [if-name]} cfg
        sock (.createSocket dgram #js {:type "udp4" :reuseAddr true})
        cfg (assoc cfg :sock sock)]
    (doto sock
      (.on "error" (fn [& err] (prn :err err)))
      (.on "message" (fn [msg rinfo] (generic-message-handler cfg msg)))
      (.on "listening" (fn []
                         (.setBroadcast sock true)
                         (socket/bind-to-device sock if-name)
                         (println "Listening on interface" if-name
                                  "port" dhcp/RECV-PORT)))
      (.bind dhcp/RECV-PORT))))
