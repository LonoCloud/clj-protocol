(ns dhcp.basic-server
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
        address (dhcp/ip-str->octet (:address srv-if-ipv4))
        netmask (dhcp/ip-str->octet (:netmask srv-if-ipv4))]
    {:raw srv-if-ipv4
     :family (:family srv-if-ipv4)
     :internal (:internal srv-if-ipv4)
     :address address
     :netmask netmask
     :broadcast (dhcp/broadcast address netmask)
     :mac (dhcp/mac-str->octet (:mac srv-if-ipv4))}))

(defn handle-message [sock msg srv-if]
  ;;(prn :pkt (-> msg .toJSON .-data js->clj))
  (let [msg-map (dhcp/read-message (-> msg .toJSON .-data js->clj))
        resp-msg-map (assoc (dhcp/default-response msg-map srv-if)
                       :yiaddr [10 0 0 77]) ;; TODO: hardcoded
        buf (js/Buffer. (clj->js (dhcp/write-message resp-msg-map)))
        resp-addr (string/join "." (:broadcast srv-if))]
    (println "Received" (:opt/msg-type msg-map) "message")
    ;;(prn :msg-map msg-map)
    ;;(prn :resp-pkt (dhcp/write-message resp-msg-map))
    ;;(prn :resp-addr resp-addr)
    (.send sock buf 0 (.-length buf) dhcp/SEND-PORT resp-addr
           #(if %1
              (println "Send failed:" %1)
              (println "Sent" (:opt/msg-type resp-msg-map)
                       "message to" (:yiaddr resp-msg-map))))))

(defn -main [ifname & args]
  (when-not ifname
    (println "Must specify an interface name")
    (.exit js/process 0))

  (let [sock (.createSocket dgram #js {:type "udp4" :reuseAddr true})
        srv-if-ipv4 (get-if-ipv4 ifname)]
    ;;(prn :srv-if-ipv4 srv-if-ipv4)
    (doto sock
      (.on "error" (fn [& err] (prn :err err)))
      (.on "message" (fn [msg rinfo] (prn :rinfo rinfo) (handle-message sock msg srv-if-ipv4)))
      (.on "listening" (fn []
                         (.setBroadcast sock true)
                         (socket/bind-to-device sock ifname)
                         (println "Listening on interface" ifname
                                  "port" dhcp/RECV-PORT)))
      (.bind dhcp/RECV-PORT))))

(set! *main-cli-fn* -main)
