(ns protocol.tlvs-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as string]
            [protocol.fields :as fields]
            [protocol.tlvs :as tlvs]))

(def TLVS-LIST ;; code, name, type,      extra-context
               [[1      :a    :tlv-seq   ]
                [2      :b    :uint8     ]
                [3      :c    :tlv-seq   ]
                [4      :d    :tlv-seq   {:tlv-tsize 1
                                          :tlv-lsize 1}]
                [5      :e    :raw       ]
                [6      :f    :bitfield  {:spec [[:bflag1  :bool   1]
                                                 [:iflag2  :int   12]
                                                 [:iflag3  :int    3]]}]])

(def TLVS-LOOKUP (tlvs/tlv-list->lookup TLVS-LIST))

(def readers (merge fields/readers
                    tlvs/readers))
(def writers (merge fields/writers
                    tlvs/writers))


;;;

(defn parse-raw-msg [s]
  (.from js/Buffer (clj->js (for [oct (string/split s #"\s+")]
                              (js/parseInt (str "0x" oct))))))

(def TLV-TEST-1
  [[:a [[:b 2]
        [:b 3]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]
   [:f {:bflag1 true
        :iflag2 1365
        :iflag3 2}]])

(def TLV-TEST-1-BUF
  (parse-raw-msg
    "00 00
    00 01 00 0a
       00 02 00 01 02
       00 02 00 01 03
    00 03 00 0c
       00 04 00 08
          05 03 01 02 03
          02 01 04
    00 06 00 02 aa aa"))

(def TLV-TEST-1-ASSOC-A-B
  [[:a [[:b 7]
        [:b 7]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]
   [:f {:bflag1 true
        :iflag2 1365
        :iflag3 2}]])

(def TLV-TEST-1-INC-A-B
  [[:a [[:b 3]
        [:b 4]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]
   [:f {:bflag1 true
        :iflag2 1365
        :iflag3 2}]])

(def TLV-TEST-1-CONJ-E
  [[:a [[:b 2]
        [:b 3]]]
   [:c [[:d [[:e [1 2 3 7]]
             [:b 4]]]]]
   [:f {:bflag1 true
        :iflag2 1365
        :iflag3 2}]])

(def TLV-TEST-1-CTX
  {:lookup TLVS-LOOKUP
   :tlv-tsize 2
   :tlv-lsize 2
   :readers readers
   :writers writers})

(deftest test-tlv-roundtrip
  (println "  test-tlv-roundtrip")
  (let [buf (.alloc js/Buffer 1500)
        end ((writers :tlv-seq) buf TLV-TEST-1 2 TLV-TEST-1-CTX)
        res-buf (.slice buf 0 end)
        res-msg ((readers :tlv-seq) res-buf 2 end TLV-TEST-1-CTX)]
    ;;(js/console.log "orig BUF:" TLV-TEST-1-BUF)
    ;;(js/console.log "res  BUF:" res-buf)
    (is (= 0 (.compare TLV-TEST-1-BUF res-buf)))
    (is (= TLV-TEST-1 res-msg))))

(deftest test-get-in-tlv
  (println "  test-get-in-tlv")
  (println "    exists")
  (is (= [2 3] (tlvs/get-in-tlv* TLV-TEST-1 [:a :b])))
  (is (= [2 3] (tlvs/get-in-tlv {:tlvs TLV-TEST-1} [:a :b])))
  (is (= [4] (tlvs/get-in-tlv* TLV-TEST-1 [:c :d :b])))
  (is (= [4] (tlvs/get-in-tlv {:foo :bar :tlvs TLV-TEST-1} [:c :d :b])))
  (println "    not exists")
  (is (= nil (tlvs/get-in-tlv* TLV-TEST-1 [:a :c])))
  (is (= nil (tlvs/get-in-tlv {:tlvs TLV-TEST-1} [:a :c])))
  )

(deftest test-assoc-in-tlv
  (println "  test-assoc-in-tlv")
  (println "    exists")
  (is (= TLV-TEST-1-ASSOC-A-B
         (tlvs/assoc-in-tlv* TLV-TEST-1 [:a :b] 7)))
  (is (= {:tlvs TLV-TEST-1-ASSOC-A-B}
         (tlvs/assoc-in-tlv {:tlvs TLV-TEST-1} [:a :b] 7)))

  (println "    not exists")
  (is (= TLV-TEST-1
         (tlvs/assoc-in-tlv* TLV-TEST-1 [:a :c] 7)))
  (is (= {:tlvs TLV-TEST-1}
         (tlvs/assoc-in-tlv {:tlvs TLV-TEST-1} [:a :c] 7))))

(deftest test-update-in-tlv
  (println "  test-update-in-tlv")
  (is (= TLV-TEST-1-INC-A-B
         (tlvs/update-in-tlv* TLV-TEST-1 [:a :b] inc)))
  (is (= {:tlvs TLV-TEST-1-INC-A-B}
         (tlvs/update-in-tlv {:tlvs TLV-TEST-1} [:a :b] inc)))
  (is (= TLV-TEST-1-CONJ-E
         (tlvs/update-in-tlv* TLV-TEST-1 [:c :d :e] conj 7)))
  (is (= {:tlvs TLV-TEST-1-CONJ-E}
         (tlvs/update-in-tlv {:tlvs TLV-TEST-1} [:c :d :e] conj 7))))

