(ns protocol.addrs-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is]]
               :clj  [clojure.test :refer [deftest is]])
            [protocol.platform :as plat]
            [protocol.fields :as fields]
            [protocol.addrs :as addrs]))

(def freaders (merge fields/readers-BE addrs/readers))
(def fwriters (merge fields/writers-BE addrs/writers))

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


(deftest test-readers
  (println "  test-readers")
  (let [buf (plat/buf-from [65 66 67 68 69 70 71 72 73 74])]
    (doseq [[t v] {:ipv4      "67.68.69.70"
                   :mac       "43:44:45:46:47:48"}]
      (println "    reader" t)
      (let [[rend res] ((freaders t) buf 2 10 {:readers freaders})]
        (is (> rend 0))
        (is res)
        (is (= v res))))))

(deftest test-writers
  (println "  test-writers")
  (doseq [[t [v1 v2 v3]]
          {:ipv4      ["67.68.69.70"       4 [0 0 67 68 69 70  0  0  0  0]]
           :mac       ["43:44:45:46:47:48" 6 [0 0 67 68 69 70 71 72  0  0]]}]
    (println "    writer" t v1 v2 v3)
    (let [buf (plat/buf-alloc 10)
          sz ((fwriters t) buf v1 2 {:writers fwriters})
          octs (plat/buf->vec buf 0)]
      (is (> sz 0))
      (is (= v3 octs)))))
