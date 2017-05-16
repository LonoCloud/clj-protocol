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

(defn server-message-handler [cfg msg rinfo]
  (let [{:keys [sock if-info disable-bcast]} cfg
        msg-map (dhcp/read-message (-> msg .toJSON .-data js->clj))
        msg-type (:opt/msg-type msg-map)
        _ (println "Received" msg-type
                   "message from" (:chaddr msg-map))
        resp-msg-map ((:message-handler cfg) cfg msg-map)
        buf (js/Buffer. (clj->js (dhcp/write-message resp-msg-map)))
        resp-addr (if (and (not disable-bcast)
                           (dhcp/MSG-TYPE-BCAST-LOOKUP msg-type))
                    (string/join "." (:broadcast (:octets if-info)))
                    (:address rinfo))]
    (.send sock buf 0 (.-length buf) dhcp/SEND-PORT resp-addr
           #(if %1
              (println "Send failed:" %1)
              (println "Sent" (:opt/msg-type resp-msg-map)
                       "message with" (:yiaddr resp-msg-map)
                       "to" resp-addr "/" (:chaddr resp-msg-map))))))

(defn first-free [ip-to-mac ranges]
  (let [ips (mapcat #(dhcp/ip-seq (:start %1) (:end %1)) ranges)]
    (first (filter #(not (contains? ip-to-mac %1)) ips))))

;; (:pool cfg) should be an atom containing:
;;     {:ranges [{:start <START-IP>
;;                :end <END-IP>}...]
;;      :ip-to-mac {<IP> <MAC>...}
;;      :mac-to-ip {<MAC> <IP>}}
(defn pool-handler [cfg msg-map]
  (let [{:keys [pool save-pool if-info]} cfg
        chaddr (dhcp/octet->mac (:chaddr msg-map))
        {:keys [ip-to-mac mac-to-ip ranges]} @pool
        cur-ip (get mac-to-ip chaddr)
        ip (or cur-ip (first-free ip-to-mac ranges))]
    (assert ip "DHCP pool exhausted")
    (println (str (and cur-ip "Re-") "Assigning") ip "to" chaddr)
    (swap! pool #(-> %1 (assoc-in [:mac-to-ip %2] %3)
                     (assoc-in [:ip-to-mac %3] %2)) chaddr ip)
    (save-pool cfg @pool)
    (assoc (dhcp/default-response msg-map (:octets if-info))
      :yiaddr (dhcp/ip->octet ip))))

(defn create-server [cfg]
  (let [{:keys [if-name]} cfg
        sock (.createSocket dgram #js {:type "udp4" :reuseAddr true})
        cfg (assoc cfg :sock sock)]
    (doto sock
      (.on "error" (fn [& err] (prn :err err)))
      (.on "message" (fn [msg rinfo]
                       (server-message-handler
                        cfg msg (js->clj rinfo :keywordize-keys true))))
      (.on "listening" (fn []
                         (.setBroadcast sock true)
                         (socket/bind-to-device sock if-name)
                         (println "Listening on interface" if-name
                                  "port" dhcp/RECV-PORT)))
      (.bind dhcp/RECV-PORT))))
