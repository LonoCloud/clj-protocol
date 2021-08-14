(ns icmp.core-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is]]
               :clj  [clojure.test :refer [deftest is]])
            [clojure.string :as string]
            [protocol.platform :as plat]
            [protocol.util :as util]
            [icmp.core :as icmp]))

(defn parse-raw-msg [s]
  (plat/buf-from (for [oct (string/split s #"\s+")]
                   (plat/string->num oct 16))))

(def ECHO-ARR [0x08, 0x00, 0x00, 0x00,
               0x00, 0x01, 0x0a, 0x09,
               0x61, 0x62, 0x63, 0x64])
(def ECHO-MSG
  {:type :echo-request
   :code 0
   :checksum 0
   :data {:id 1, :seq-num 2569,
          :payload [97 98 99 100]}})

(def REDIRECT-ARR
  [0x05, 0x01, 0x01, 0x01,
   0x0a, 0x00, 0x00, 0x01,
   0x61, 0x62, 0x63, 0x64])

(def REDIRECT-MSG
  {:type :redirect
   :code 1
   :checksum 257
   :data {:gw-addr "10.0.0.1"
          :orig-packet [0x61 0x62 0x63 0x64]}})


(deftest test-icmp-echo
  (println "  test-icmp-echo")
  (let [echo-buf (plat/buf-from ECHO-ARR)
        msg1 (icmp/read-icmp echo-buf)
        ;;_ (prn :msg1 msg1)
        buf (icmp/write-icmp msg1)
        msg2 (icmp/read-icmp buf)]
    (is (= ECHO-MSG msg1))
    ;;(println (util/pr-buf echo-buf {:prefix "echo-buf:  "}))
    ;;(println (util/pr-buf buf      {:prefix "buf:       "}))
    (is (= 0 (plat/buf-cmp echo-buf buf)))
    (is (= ECHO-MSG msg2))))

(deftest test-icmp-reply
  (println "  test-icmp-reply")
  (let [reply-buf (plat/buf-from (assoc ECHO-ARR 0 0))
        msg (icmp/read-icmp reply-buf)
        buf (icmp/write-icmp msg)
        msg2 (icmp/read-icmp buf)]
    (is (= (assoc ECHO-MSG :type :echo-reply) msg))
    (is (= 0 (plat/buf-cmp reply-buf buf)))
    (is (= (assoc ECHO-MSG :type :echo-reply) msg2))))

(deftest test-icmp-redirect
  (println "  test-icmp-redirect")
  (let [redirect-buf (plat/buf-from REDIRECT-ARR)
        msg (icmp/read-icmp redirect-buf)
        buf (icmp/write-icmp msg)
        msg2 (icmp/read-icmp buf)]
    (is (= REDIRECT-MSG msg))
    (is (= 0 (plat/buf-cmp redirect-buf buf)))
    (is (= REDIRECT-MSG msg2))))
