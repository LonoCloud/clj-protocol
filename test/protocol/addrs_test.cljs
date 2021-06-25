(ns protocol.addrs-test
  (:require [cljs.test :refer-macros [deftest is]]
            [protocol.addrs :as addrs]))

(deftest test-net-addr-functions
  (println "  test-net-addr-functions")

  (println "    octet->X")
  (is (= "0b:0c:0d:0e:0f:10" (addrs/octet->mac [11 12 13 14 15 16])))
  (is (= "11.12.13.14"       (addrs/octet->ip [11 12 13 14])))

  (println "    X->octet")
  (is (= [11 12 13 14 15 16] (addrs/mac->octet "0b:0c:0d:0e:0f:10")))
  (is (= [11 12 13 14]       (addrs/ip->octet "11.12.13.14")))

  (println "    first-ip")
  (is (= "10.0.0.0" (addrs/first-ip "10.0.0.7" "255.255.255.0")))
  (is (= "10.8.0.0" (addrs/first-ip "10.8.20.7" "255.255.0.0")))

  (println "    broadcast")
  (is (= "10.0.0.255"   (addrs/broadcast "10.0.0.7" "255.255.255.0")))
  (is (= "10.0.255.255" (addrs/broadcast "10.0.0.7" "255.255.0.0")))

  (println "    mask-ip->prefix")
  (is (= 24 (addrs/mask-ip->prefix "255.255.255.0")))
  (is (= 16 (addrs/mask-ip->prefix "255.255.0.0")))
  (is (= 18 (addrs/mask-ip->prefix "255.255.192.0")))

  (println "    network-start-end")
  (is (= ["10.0.0.1" "10.0.255.254"]
         (addrs/network-start-end "10.0.1.7" "255.255.0.0" true)))
  (is (= ["10.0.0.0" "10.0.255.255"]
         (addrs/network-start-end "10.0.1.7" "255.255.0.0" false)))

  (println "    ip-seq")
  (is (= ["10.0.1.7" "10.0.1.8" "10.0.1.9" "10.0.1.10"]
         (addrs/ip-seq "10.0.1.7" "10.0.1.10")))
  )

