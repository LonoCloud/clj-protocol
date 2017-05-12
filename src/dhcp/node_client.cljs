(ns dhcp.node-client
  (:require [dhcp.core :as dhcp]
            ;; Move get-if-ipv4 to more generic location
            [dhcp.node-server :as server]))

(def dgram (js/require "dgram"))

(defn client-message-handler [cfg msg resp-addr]
  (let [{:keys [sock]} cfg
        resp-ip (dhcp/octet->ip resp-addr)
        msg-map (when msg
                  (dhcp/read-message (-> msg .toJSON .-data js->clj)))
        msg-type (:opt/msg-type msg-map)
        resp-msg-map (condp = msg-type
                       nil    {:op 1 :opt/msg-type :DISCOVER}
                       :OFFER {:op 1 :opt/msg-type :REQUEST}
                       :ACK   nil
                       :NACK (throw (js/Error. "Received NACK"))
                       (throw (js/Error. (str "Received invalid msg type"
                                              msg-type))))]
    (when msg-map
      (println "Received" msg-type
               "message from" (dhcp/octet->ip (:siaddr msg-map))))
    (if resp-msg-map
      (let [buf (js/Buffer. (clj->js (dhcp/write-message resp-msg-map)))]
        (.send sock buf 0 (.-length buf) dhcp/RECV-PORT resp-ip
               #(if %1
                  (println "Send failed:" %1)
                  (println "Sent" (:opt/msg-type resp-msg-map)
                           "to" resp-ip))))
      (println "Got address:" (dhcp/octet->ip (:yiaddr msg-map))))))

;; TODO: - lease renewal
;;       - sending out MAC
(defn -main [if-name & [server-ip & args]]
  (when-not if-name
    (println "Must specify an interface name")
    (.exit js/process 0))

  (let [if-info (server/get-if-ipv4 if-name)
        bcast-addr (:broadcast (:octets if-info))
        server-addr (if server-ip (dhcp/ip->octet server-ip) bcast-addr)
        sock (.createSocket dgram #js {:type "udp4" :reuseAddr true})
        cfg {:sock sock}
        sock (doto sock
               (.on "listening" (fn []
                                  (when (= bcast-addr server-addr)
                                    (.setBroadcast sock true))))
               (.on "message" (fn [msg rinfo]
                                (client-message-handler cfg msg server-addr)))
               (.bind dhcp/SEND-PORT))]
    (client-message-handler cfg nil server-addr)))
