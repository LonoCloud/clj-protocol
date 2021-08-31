;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.core-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is]]
               :clj  [clojure.test :refer [deftest is]])
            [clojure.string :as string]
            [protocol.platform :as plat]
            [protocol.util :as util]
            [dhcp.core :as dhcp]))

(defn parse-raw-msg [s]
  (plat/buf-from (for [oct (string/split s #"\s+")]
                   (plat/string->num oct 16))))

(def BASIC-MSG-STR
  (str
    "02 01 06 00 " ;; op, htype, hlen, hops
    "00 00 00 07 " ;; xid
    "00 08 00 00 " ;; secs, flags
    "00 00 00 00 " ;; ciaddr
    "00 00 00 00 " ;; yiaddr
    "00 00 00 00 " ;; siaddr
    "05 06 07 08 " ;; giaddr
    "01 02 03 04 06 07 " ;; chaddr
    "00 00 00 00 00 00 00 00 00 00 " ;; chaddr-extra
    "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 " ;; sname
    "41 42 43 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 " ;; bootfile
    "63 82 53 63 " ;; cookie
    "35 01 02 " ;; msg-type
    "ff" ;; end
  ))

(def BASIC-MSG-BUF (parse-raw-msg BASIC-MSG-STR))

(def BASIC-MSG-MAP
  {
   :op 2, :htype 1, :hlen 6, :hops 0,
   :xid 7,
   :secs 8, :flags {:broadcast false :reserved 0},
   :ciaddr "0.0.0.0",
   :yiaddr "0.0.0.0",
   :siaddr "0.0.0.0",
   :giaddr "5.6.7.8",
   :chaddr "01:02:03:04:06:07",
   :chaddr-extra [0 0 0 0 0 0 0 0 0 0],
   :sname "",
   :bootfile "ABC",
   :cookie [99 130 83 99],
   :opt/msg-type :OFFER})

(def DEFAULT-MSG
  (merge
    (dhcp/default-response
      {:opt/msg-type :DISCOVER
       :xid 7
       :secs 8
       :chaddr "01:02:03:04:06:07"}
      {:address "5.6.7.8"
       :netmask "255.255.255.0"
       :broadcast "5.6.7.255"})))


(deftest test-basic-dhcp
  (println "  test-basic-dhcp")
  (let [msg (dhcp/read-dhcp BASIC-MSG-BUF)
        ;;_ (prn :msg msg)
        buf (dhcp/write-dhcp msg)]
    (is (= BASIC-MSG-MAP msg))
    ;;(println (util/pr-bufs [BASIC-MSG-BUF buf] {:prefix "  "}))
    (is (= 0 (plat/buf-cmp BASIC-MSG-BUF buf)))))

(deftest test-default-dhcp
  (println "  test-default-dhcp")
  (let [msg-before DEFAULT-MSG
        ;;_ (prn :msg-before msg-before)
        buf (dhcp/write-dhcp DEFAULT-MSG)
        ;;_ (println (util/pr-buf buf {:prefix "  "}))
        msg-after (dhcp/read-dhcp buf)]
    ;;(prn :before (sort (keys msg-before)))
    ;;(prn :after- (sort (keys msg-after)))
    (is (= (sort (keys msg-before))
           (sort (keys msg-after))))
    (is (= msg-before msg-after))
    (doseq [[k v] msg-before]
      (is (= (get msg-before k) (get msg-after k))))))

(deftest test-relay-agent-opt
  (println "  test-relay-agent-opt")
  (let [relay-agent-info {:circuit-id [1 2 3 4]
                          :remote-id [5 6 7 8 9 10]
                         :subscriber-id "subscriber-string"}
        etherboot {:eb-priority 2
                   :eb-username "xyz"}
        msg-before (assoc DEFAULT-MSG
                          :opt/relay-agent-info relay-agent-info
                          :opt/etherboot etherboot)
        buf (dhcp/write-dhcp msg-before)
        msg-after (dhcp/read-dhcp buf)]
    (is (= msg-before msg-after))))
