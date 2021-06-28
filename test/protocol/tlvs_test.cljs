(ns protocol.tlvs-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as string]
            [protocol.fields :as fields]
            [protocol.tlvs :as tlvs]))

(def TLVS-LIST ;; code, name, type
               [[1      :a    :tlvs ]
                [2      :b    :uint8]
                [3      :c    :tlvs ]
                [4      :d    :tlvs ]
                [5      :e    :raw  ]])

(def TLVS-LOOKUP (tlvs/tlv-list->lookup TLVS-LIST))

(def readers (merge fields/readers
                    tlvs/readers
                    {:tlvs #(tlvs/read-tlv-seq %1 %2 %3 readers TLVS-LOOKUP 2 2)}))
(def writers (merge fields/writers
                    tlvs/writers
                    {:tlvs #(tlvs/write-tlv-seq %1 %2 %3 writers TLVS-LOOKUP 2 2)}))


;;;

(defn parse-raw-msg [s]
  (.from js/Buffer (clj->js (for [oct (string/split s #"\s+")]
                              (js/parseInt (str "0x" oct))))))

(def TLV-TEST-1
  [[:a [[:b 2]
        [:b 3]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]])

(def TLV-TEST-1-BUF
  (parse-raw-msg "00 00 00 01 00 0a 00 02 00 01 02 00 02 00 01 03 00 03 00 10 00 04 00 0c 00 05 00 03 01 02 03 00 02 00 01 04"))

(def TLV-TEST-1-ASSOC-A-B
  [[:a [[:b 7]
        [:b 7]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]])

(def TLV-TEST-1-INC-A-B
  [[:a [[:b 3]
        [:b 4]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]])

(def TLV-TEST-1-CONJ-E
  [[:a [[:b 2] [:b 3]]]
   [:c [[:d [[:e [1 2 3 7]] [:b 4]]]]]])

(deftest test-tlv-roundtrip
  (println "  test-tlv-roundtrip")
  (let [buf (.alloc js/Buffer 1500)
        end ((writers :tlvs) buf TLV-TEST-1 2)
        res-buf (.slice buf 0 end)
        res-msg ((readers :tlvs) res-buf 2 end)]
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

