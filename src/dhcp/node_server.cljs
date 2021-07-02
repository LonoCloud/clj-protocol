(ns dhcp.node-server
  (:require [cljs.nodejs :as nodejs]
            [clojure.string :as string]
            [protocol.fields :as fields]
            [protocol.socket :as socket]
            [dhcp.core :as dhcp]))

(nodejs/enable-util-print!)

(def dgram (js/require "dgram"))
(def os (js/require "os"))

(defn get-if-ipv4 [if-name]
  (let [srv-ifs (-> os .networkInterfaces (js->clj :keywordize-keys true))
        srv-if-ipv4 (-> srv-ifs (get (keyword if-name)) first)
        {:keys [address netmask]} srv-if-ipv4]
    (assoc srv-if-ipv4
      :octets {:address (fields/ip->octet address)
               :netmask (fields/ip->octet netmask)
               :broadcast (fields/ip->octet (fields/broadcast address netmask))
               :mac (fields/mac->octet (:mac srv-if-ipv4))})))

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
                    (string/join "." (:broadcast (:octets if-info)))
                    (:address rinfo))]
    (.send sock buf 0 (.-length buf) (:port rinfo) resp-addr
           #(if %1
              (println "Send failed:" %1)
              (when-let [logfn (:log-msg cfg)]
                (logfn cfg resp-msg-map resp-addr))))))

(defn first-free [ip-to-mac ranges]
  (let [ips (mapcat #(fields/ip-seq (:start %1) (:end %1)) ranges)]
    (first (filter #(not (contains? ip-to-mac %1)) ips))))

;; (:pool cfg) should be an atom containing:
;;     {:ranges [{:start <START-IP>
;;                :end <END-IP>}...]
;;      :ip-to-mac {<IP> <MAC>...}
;;      :mac-to-ip {<MAC> <IP>}}
(defn pool-handler [cfg msg-map]
  (let [{:keys [pool save-pool if-info]} cfg
        chaddr (fields/octet->mac (:chaddr msg-map))
        {:keys [ip-to-mac mac-to-ip ranges]} @pool
        cur-ip (get mac-to-ip chaddr)
        ip (or cur-ip (first-free ip-to-mac ranges))]
    (assert ip "DHCP pool exhausted")
    (println (str (and cur-ip "Re-") "Assigning") ip "to" chaddr)
    (swap! pool #(-> %1 (assoc-in [:mac-to-ip %2] %3)
                     (assoc-in [:ip-to-mac %3] %2)) chaddr ip)
    (save-pool cfg @pool)
    (assoc (dhcp/default-response msg-map (:octets if-info))
      :yiaddr (fields/ip->octet ip))))

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
                         (socket/reuse-port sock)
                         (.setBroadcast sock true)
                         (when buffsz (socket/set-rcvbuf sock buffsz))
                         (socket/bind-to-device sock if-name)
                         (println "Listening on interface" if-name
                                  "port" dhcp/RECV-PORT)))
      (.bind dhcp/RECV-PORT))))
