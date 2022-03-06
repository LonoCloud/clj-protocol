;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns pcap.core-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is]]
               :clj  [clojure.test :refer [deftest is]])
            [pcap.core :as pcap]))

(def EXAMPLE-PCAP-FILE "test/example.pcap")

(def FIRST-IP
  {:ver-ihl {:version 4
             :ihl 5}
   :tos 0
   :length 57
   :id 30889
   :flags-frag 16384
   :ttl 64
   :protocol 17
   :dst-addr "8.8.8.8"
   :src-addr "192.168.88.2"})

(def LAST-IP
  {:ver-ihl {:version 4
             :ihl 5}
   :tos 0
   :length 56
   :id 0
   :flags-frag 0
   :ttl 102
   :protocol 1
   :dst-addr "192.168.88.2"
   :src-addr "35.234.126.59"})

(deftest test-example-file
  (println "  test-example-file")
  (let [trace (pcap/parse-file EXAMPLE-PCAP-FILE)
        records (:records trace)
        ptypes (group-by (comp :ethertype :packet) records)
        first-ip (-> records (nth 2) :packet :payload (dissoc :checksum :payload))
        last-ip (-> records last :packet :payload (dissoc :checksum :payload))]
    (is (= 0xa1b2c3d4 (:magic trace)))
    (is (= 25 (count records)))
    (is (= 2 (count (get ptypes 2054))))
    (is (= 23 (count (get ptypes 2048))))
    (is (= FIRST-IP first-ip))
    (is (= LAST-IP last-ip))))
