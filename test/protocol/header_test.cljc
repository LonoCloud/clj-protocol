;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns protocol.header-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is]]
               :clj  [clojure.test :refer [deftest is]])
            [clojure.string :as string]
            [protocol.platform :as plat]
            [protocol.fields :as fields]
            [protocol.tlvs :as tlvs]
            [protocol.header :as header]
            [protocol.util :as util]))

(defn parse-raw-msg [s]
  (plat/buf-from (for [oct (string/split s #"\s+")]
                   (plat/string->num oct 16))))

(def MSG-TYPE-LIST [[1 :MSG1] [2 :MSG2]])
(def MSG-TYPE-LOOKUP (fields/list->lookup MSG-TYPE-LIST [0 1] [1 0]))

(def TLV-LIST
  [;; code, name,             type,     extra-context
   [1       :tlv/msg-type     :lookup   {:lookup-type :uint8
                                         :lookup MSG-TYPE-LOOKUP}]
   [2       :tlv/address      :raw      ]
   [3       :tlv/hostname     :utf8     ]
   [4       :tlv/hops         :uint8    ]])
(def TLV-LOOKUP (tlvs/tlv-list->lookup TLV-LIST))

(def HEADER-1
  ;; name,  type,         default,  extra context
  [[:op     :uint8        {:default 0}]
   [:xid    :uint32       {:default 0}]
   [:flags  :bitfield     {:length 2
                           :spec [[:bflag1  :bool   1]
                                  [:iflag2  :int   12]
                                  [:iflag3  :int    3]]}]
   [:tlvs   :tlv-map      {:lookup TLV-LOOKUP
                           :tlv-tsize 1
                           :tlv-lsize 1}]])

(def TEST-MSG-1-STR
  "01
   00 00 00 02
   aa aa
   01 01 01
   02 04 06 07 08 09
   03 05 41 42 43 44 45")
(def TEST-MSG-1-BUF (parse-raw-msg TEST-MSG-1-STR))
(def TEST-MSG-1-MAP {:op 1
                     :xid 2
                     :flags {:bflag1 true
                             :iflag2 1365
                             :iflag3 2}
                     :tlvs {:tlv/msg-type :MSG1
                            :tlv/address [6 7 8 9]
                            :tlv/hostname "ABCDE"}})

;;;

(def HEADER-2
  ;; name,  type,         default,  extra context
  [[:op     :uint16       {:default 0}]
   [:host   :utf8         {:length 6
                           :default "abcdef"}]
   [:hops   :tlv          {:lookup TLV-LOOKUP
                           :tlv-tsize 2
                           :tlv-lsize 2}]
   [:tlvs   :tlv-seq      {:lookup TLV-LOOKUP
                           :tlv-tsize 2
                           :tlv-lsize 2}]])

(def TEST-MSG-2-STR
  "00 03
   61 62 63 64 65 66
   00 04 00 01 11
   00 01 00 01 02
   00 02 00 04 06 07 08 09
   00 02 00 04 03 04 05 06")
(def TEST-MSG-2-BUF (parse-raw-msg TEST-MSG-2-STR))
(def TEST-MSG-2-MAP {:op 3
                     :host "abcdef"
                     :hops [:tlv/hops 17]
                     :tlvs [[:tlv/msg-type :MSG2]
                            [:tlv/address [6 7 8 9]]
                            [:tlv/address [3 4 5 6]]]})


;;;

;;  name,   type,         length,  default,  extra context
(def HEADER-3b
  [[:hops   :tlv          {:lookup TLV-LOOKUP
                           :tlv-tsize 2
                           :tlv-lsize 2}]])
(def HEADER-3c
  [[:host   :utf8         {:length 6
                           :default "abcdef"}]])
(def HEADER-3
  [[:op     :uint8        {:default 0}]
   [:data-b :header       {:spec HEADER-3b}]
   [:data-c :header       {:spec HEADER-3c}]])

(def TEST-MSG-3-STR
  "01
   00 04 00 01 11
   61 62 63 64 65 66")
(def TEST-MSG-3-BUF (parse-raw-msg TEST-MSG-3-STR))
(def TEST-MSG-3-MAP {:op 1
                     :data-b {:hops [:tlv/hops 17]}
                     :data-c {:host "abcdef"}})

;;;

(def HEADER-4-PACKET
  ;;name,       type,      extra-context
  [[:field2    :uint8     {:default 0}]
   [:payload   :raw       {:default 0}]])

(def HEADER-4-RECORD
  ;;name,       type,      extra-context
  [[:pkt-len   :uint8     {:default 0}]
   [:packet    :header    {:length :pkt-len
                           :spec HEADER-4-PACKET}]])

(def HEADER-4
  ;;name,       type,      extra-context
  [[:field1     :uint8     {:default 0}]
   [:records    :loop      {:loop-type :header
                            :spec HEADER-4-RECORD}]])

(def TEST-MSG-4-STR
  "71
   04  72 02 03 04
   05  73 05 06 07 08
   02  74 09")
(def TEST-MSG-4-BUF (parse-raw-msg TEST-MSG-4-STR))
(def TEST-MSG-4-MAP {:field1 0x71
                     :records [{:pkt-len 4
                                :packet {:field2 0x72
                                         :payload [0x02 0x03 0x04]}}
                               {:pkt-len 5
                                :packet {:field2 0x73
                                         :payload [0x05 0x06 0x07 0x08]}}
                               {:pkt-len 2
                                :packet {:field2 0x74
                                         :payload [0x09]}}]})
;;;;;;;;;;;;;;;;;


(def readers (merge fields/readers-BE tlvs/readers header/readers))
(def writers (merge fields/writers-BE tlvs/writers header/writers))

(def TEST-CTX-1 {:readers readers :writers writers :spec HEADER-1})
(def TEST-CTX-2 {:readers readers :writers writers :spec HEADER-2})
(def TEST-CTX-3 {:readers readers :writers writers :spec HEADER-3})
(def TEST-CTX-4 {:readers readers :writers writers :spec HEADER-4})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-header-read
  (println "  test-header-read")
  (let [[rend1 msg-map1] (header/read-header TEST-MSG-1-BUF 0 TEST-CTX-1)
        ;;_ (prn :msg-map1 msg-map1)
        [rend2 msg-map2] (header/read-header TEST-MSG-2-BUF 0 TEST-CTX-2)
        ;;_ (prn :msg-map2 msg-map2)
        [rend3 msg-map3] (header/read-header TEST-MSG-3-BUF 0 TEST-CTX-3)
        ;;_ (prn :msg-map3 msg-map3)
        [rend4 msg-map4] (header/read-header TEST-MSG-4-BUF 0 TEST-CTX-4)
        ;;_ (prn :msg-map4 msg-map4)
        ]
    (is (= TEST-MSG-1-MAP msg-map1))
    (is (= TEST-MSG-2-MAP msg-map2))
    (is (= TEST-MSG-3-MAP msg-map3))
    (is (= TEST-MSG-4-MAP msg-map4))
    ))

(deftest test-header-write
  (println "  test-header-write")
  (let [msg-buf1 (header/write-header-full nil TEST-MSG-1-MAP 0 TEST-CTX-1)
        msg-buf2 (header/write-header-full nil TEST-MSG-2-MAP 0 TEST-CTX-2)
        msg-buf3 (header/write-header-full nil TEST-MSG-3-MAP 0 TEST-CTX-3)
        msg-buf4 (header/write-header-full nil TEST-MSG-4-MAP 0 TEST-CTX-4)
        ]
    ;;(println (util/pr-buf TEST-MSG-1-BUF {:prefix "TEST-MSG-1-BUF: "}))
    ;;(println (util/pr-buf msg-buf1       {:prefix "      msg-buf1: "}))
    ;;(println (util/pr-buf TEST-MSG-2-BUF {:prefix "TEST-MSG-2-BUF: "}))
    ;;(println (util/pr-buf msg-buf2       {:prefix "      msg-buf2: "}))
    ;;(println (util/pr-buf TEST-MSG-3-BUF {:prefix "TEST-MSG-3-BUF: "}))
    ;;(println (util/pr-buf msg-buf3       {:prefix "      msg-buf3: "}))
    ;;(println "TEST-MSG-2-BUF | msg-buf2:")
    ;;(println (util/pr-bufs [TEST-MSG-2-BUF msg-buf2] {:prefix "  "}))
    (is (> (plat/buf-len msg-buf1) 0))
    (is (> (plat/buf-len msg-buf2) 0))
    (is (> (plat/buf-len msg-buf3) 0))
    (is (> (plat/buf-len msg-buf4) 0))
    (is (= 0 (plat/buf-cmp TEST-MSG-1-BUF msg-buf1)))
    (is (= 0 (plat/buf-cmp TEST-MSG-2-BUF msg-buf2)))
    (is (= 0 (plat/buf-cmp TEST-MSG-3-BUF msg-buf3)))
    (is (= 0 (plat/buf-cmp TEST-MSG-4-BUF msg-buf4)))))

(deftest test-header-roundtrip
  (println "  test-header-roundtrip")
  (let [msg-buf1 (header/write-header-full nil TEST-MSG-1-MAP 0 TEST-CTX-1)
        msg-map1 (header/read-header-full msg-buf1 0 TEST-CTX-1)
        msg-buf2 (header/write-header-full nil TEST-MSG-2-MAP 0 TEST-CTX-2)
        msg-map2 (header/read-header-full msg-buf2 0 TEST-CTX-2)
        msg-buf3 (header/write-header-full nil TEST-MSG-3-MAP 0 TEST-CTX-3)
        msg-map3 (header/read-header-full msg-buf3 0 TEST-CTX-3)
        msg-buf4 (header/write-header-full nil TEST-MSG-4-MAP 0 TEST-CTX-4)
        msg-map4 (header/read-header-full msg-buf4 0 TEST-CTX-4)]
    (is (= TEST-MSG-1-MAP msg-map1))
    (is (= TEST-MSG-2-MAP msg-map2))
    (is (= TEST-MSG-3-MAP msg-map3))
    (is (= TEST-MSG-4-MAP msg-map4))))

