(ns icmp.ping
  (:require [protocol.fields :as fields]
            [protocol.tlvs :as tlvs]
            [protocol.header :as header]
            [icmp.core :as icmp]))

(def raw (js/require "raw-socket"))

(def last-seq-num (atom 0))

(defn ping [sock targ-ip]
  (let [seq-num (swap! last-seq-num inc)
        ping-msg {:type :echo-request
                  :code 0
                  :data {:id 7
                         :seq-num seq-num
                         :payload [0xbe 0xef 0xfe 0xed]}}
        buf (icmp/write-icmp ping-msg)]
    (println ">>>" targ-ip ping-msg)
    ;;(js/console.log "ping buf:" buf)
    (.writeChecksum raw buf 2 (.createChecksum raw buf))
    (.send sock buf 0 (.-length buf) targ-ip
           (fn [err byts]
             (if err
               (js/console.log "error sending ping:" err)
               #_(js/console.log "sent" byts))))))

(defn main [targ-ip & args]
  (let [sock (.createSocket raw #js {:protocol (.-Protocol.ICMP raw)})]
    (doto sock
      (.on "message" (fn [buf srv]
                       ;; We get whole IP packet
                       (let [icmp-buf (.slice buf 20)
                             msg-map (icmp/read-icmp icmp-buf)]
                         ;;(js/console.log "on message UDP buf:" udp-buf)
                         (println "<<<" srv msg-map)))))
    (js/setInterval #(ping sock targ-ip) 2000)))
