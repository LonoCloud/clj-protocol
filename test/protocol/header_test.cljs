(ns protocol.header-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as string]
            [protocol.fields :as fields]
            [protocol.tlvs :as tlvs]
            [protocol.header :as header]))

(defn parse-raw-msg [s]
  (.from js/Buffer (clj->js (for [oct (string/split s #"\s+")]
                              (js/parseInt (str "0x" oct))))))

(def TLV-LIST
  [;; code, name,             type
   [1       :tlv/msg-type     :msg-type ]
   [2       :tlv/address      :raw      ]
   [3       :tlv/hostname     :str      ]
   [4       :tlv/hops         :uint8    ]])
(def TLV-LOOKUP (tlvs/tlv-list->lookup TLV-LIST))

(def HEADER-1
  ;; name,  type,         length,  default,  extra context
  [[:op     :uint8        1        {:default 0}]
   [:xid    :uint32       4        {:default 0}]
   [:flags  :bitfield     2        {:spec [[:bflag1  :bool   1]
                                           [:iflag2  :int   12]
                                           [:iflag3  :int    3]]}]
   [:tlvs   :tlv-map      :*       {:lookup TLV-LOOKUP
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
  ;; name,  type,         length,  default,  extra context
  [[:op     :uint16       2        {:default 0}]
   [:host   :str          6        {:default "abcdef"}]
   [:hops   :tlv          :*       {:lookup TLV-LOOKUP
                                    :tlv-tsize 2
                                    :tlv-lsize 2}]
   [:tlvs   :tlv-seq      :*       {:lookup TLV-LOOKUP
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
  [[:hops   :tlv          :*       {:lookup TLV-LOOKUP
                                    :tlv-tsize 2
                                    :tlv-lsize 2}]])
(def HEADER-3c
  [[:host   :str          6        {:default "abcdef"}]])
(def HEADER-3 
  [[:op     :uint8        1        {:default 0}]
   [:data-b :header       :*       {:spec HEADER-3b}]
   [:data-c :header       :*       {:spec HEADER-3c}]])

(def TEST-MSG-3-STR
  "01
   00 04 00 01 11
   61 62 63 64 65 66")
(def TEST-MSG-3-BUF (parse-raw-msg TEST-MSG-3-STR))
(def TEST-MSG-3-MAP {:op 1
                     :data-b {:hops [:tlv/hops 17]}
                     :data-c {:host "abcdef"}})

;;;;;;;;;;;;;;;;;


(def MSG-TYPE-LIST [[1 :MSG1] [2 :MSG2]])
(def MSG-TYPE-LOOKUP
  (merge (into {} (map (fn [[n m]] [n m]) MSG-TYPE-LIST))
         (into {} (map (fn [[n m]] [m n]) MSG-TYPE-LIST))))

(set! *warn-on-infer* false)

(def readers
  (merge fields/readers-BE
         tlvs/readers
         header/readers
         {:msg-type #(get MSG-TYPE-LOOKUP (.readUInt8 %1 %2))}))

(def writers
  (merge fields/writers-BE
         tlvs/writers
         header/writers
         {:msg-type #(.writeUInt8 %1 (get MSG-TYPE-LOOKUP %2) %3)}))

(set! *warn-on-infer* true)

(def TEST-CTX-1 {:readers readers :writers writers :spec HEADER-1})
(def TEST-CTX-2 {:readers readers :writers writers :spec HEADER-2})
(def TEST-CTX-3 {:readers readers :writers writers :spec HEADER-3})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-header-read
  (println "  test-header-read")
  (let [msg-map1 (header/read-header TEST-MSG-1-BUF 0 nil TEST-CTX-1)
        msg-map2 (header/read-header TEST-MSG-2-BUF 0 nil TEST-CTX-2)
        msg-map3 (header/read-header TEST-MSG-3-BUF 0 nil TEST-CTX-3)]
    ;;(prn :msg-map1 msg-map1)
    ;;(prn :msg-map2 msg-map2)
    ;;(prn :msg-map3 msg-map3)
    (is (= TEST-MSG-1-MAP msg-map1))
    (is (= TEST-MSG-2-MAP msg-map2))
    (is (= TEST-MSG-3-MAP msg-map3))))

(deftest test-header-write
  (println "  test-header-write")
  (let [msg-buf1 (header/write-header-full nil TEST-MSG-1-MAP 0 TEST-CTX-1)
        msg-buf2 (header/write-header-full nil TEST-MSG-2-MAP 0 TEST-CTX-2)
        msg-buf3 (header/write-header-full nil TEST-MSG-3-MAP 0 TEST-CTX-3)]
    ;;(println (util/pr-buf TEST-MSG-1-BUF {:prefix "TEST-MSG-1-BUF: "}))
    ;;(println (util/pr-buf msg-buf1       {:prefix "      msg-buf1: "}))
    ;;(println (util/pr-buf TEST-MSG-2-BUF {:prefix "TEST-MSG-2-BUF: "}))
    ;;(println (util/pr-buf msg-buf2       {:prefix "      msg-buf2: "}))
    ;;(println (util/pr-buf TEST-MSG-3-BUF {:prefix "TEST-MSG-3-BUF: "}))
    ;;(println (util/pr-buf msg-buf3       {:prefix "      msg-buf3: "}))
    ;;(println "TEST-MSG-2-BUF | msg-buf2:")
    ;;(println (util/pr-bufs [TEST-MSG-2-BUF msg-buf2] {:prefix "  "}))
    (is (> (.-length msg-buf1) 0))
    (is (> (.-length msg-buf2) 0))
    (is (> (.-length msg-buf3) 0))
    (is (= 0 (.compare TEST-MSG-1-BUF msg-buf1)))
    (is (= 0 (.compare TEST-MSG-2-BUF msg-buf2)))
    (is (= 0 (.compare TEST-MSG-3-BUF msg-buf3)))))

(deftest test-header-roundtrip
  (println "  test-header-roundtrip")
  (let [msg-buf1 (header/write-header-full nil TEST-MSG-1-MAP 0 TEST-CTX-1)
        msg-map1 (header/read-header-full msg-buf1 0 nil TEST-CTX-1)
        msg-buf2 (header/write-header-full nil TEST-MSG-2-MAP 0 TEST-CTX-2)
        msg-map2 (header/read-header-full msg-buf2 0 nil TEST-CTX-2)
        msg-buf3 (header/write-header-full nil TEST-MSG-3-MAP 0 TEST-CTX-3)
        msg-map3 (header/read-header-full msg-buf3 0 nil TEST-CTX-3)]
    (is (= TEST-MSG-1-MAP msg-map1))
    (is (= TEST-MSG-2-MAP msg-map2))
    (is (= TEST-MSG-3-MAP msg-map3))))

