(ns icmp.ping
  (:require [protocol.fields :as fields]
            [protocol.tlvs :as tlvs]
            [protocol.header :as header]
            [icmp.core :as icmp]
            ["raw-socket" :as raw]))

(def ^:private last-seq-num (atom 0))

(defn ping
  "Send a ping to `targ-ip` using socket `sock`"
  [sock targ-ip]
  (let [seq-num (swap! last-seq-num inc)
        ping-msg {:type :echo-request
                  :code 0
                  :data {:id 7
                         :seq-num seq-num
                         :payload [0xbe 0xef 0xfe 0xed]}}
        buf (icmp/write-icmp ping-msg)]
    (println ">>>" targ-ip ping-msg)
    ;;(js/console.log "ping buf:" buf)
    (raw/writeChecksum buf 2 (raw/createChecksum buf))
    (.send sock buf 0 (.-length buf) targ-ip
           (fn [err byts]
             (if err
               (js/console.log "error sending ping:" err)
               #_(js/console.log "sent" byts))))))

(defn main
  "Create a raw socket and start sending periodic pings to `targ-ip`
  and printing the responses."
  [targ-ip & args]
  (let [sock (raw/createSocket #js {:protocol raw/Protocol.ICMP})]
    (doto sock
      (.on "message" (fn [buf srv]
                       ;; We get whole IP packet
                       (let [icmp-buf (.slice buf 20)
                             msg-map (icmp/read-icmp icmp-buf)]
                         ;;(js/console.log "on message UDP buf:" udp-buf)
                         (println "<<<" srv msg-map)))))
    (js/setInterval #(ping sock targ-ip) 2000)))
