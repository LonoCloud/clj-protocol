(ns pcap.core-test
  (:require [cljs.test :refer-macros [deftest is]]
            [pcap.core :as pcap]))

(def EXAMPLE-PCAP-FILE "example.pcap")

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

(deftest test-example-file
  (println "  test-example-file")
  (let [trace (pcap/parse-file EXAMPLE-PCAP-FILE)
        records (:records trace)
        ptypes (group-by (comp :ethertype :packet) records)
        ip (-> records (nth 2) :packet :payload (dissoc :checksum :payload))]
    (is (= 0xa1b2c3d4 (:magic trace)))
    (is (= 25 (count records)))
    (is (= 2 (count (get ptypes 2054))))
    (is (= 23 (count (get ptypes 2048))))
    (is (= FIRST-IP ip))
    ))
