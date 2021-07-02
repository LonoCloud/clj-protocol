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
                                                 [:iflag3  :int    3]]}]
                [255    :stop :tlv-stop  ]])

(def TLVS-LOOKUP (tlvs/tlv-list->lookup TLVS-LIST))

(def readers (merge fields/readers-BE
                    tlvs/readers))
(def writers (merge fields/writers-BE
                    tlvs/writers))


;;;

(defn parse-raw-msg [s]
  (.from js/Buffer (clj->js (for [oct (string/split s #"\s+")]
                              (js/parseInt (str "0x" oct))))))

(def TLV-TEST-1
  [:b 2])

(def TLV-TEST-1-STR
  (str "00 00 " ;; padding
       "00 02 00 01 02"))
(def TLV-TEST-1-BUF (parse-raw-msg TLV-TEST-1-STR))

(def TLVS-TEST-1
  [[:a [[:b 2]
        [:b 3]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]
   [:f {:bflag1 true
        :iflag2 1365
        :iflag3 2}]])

(def TLVS-TEST-1-STR
  (str "00 00 "                 ;; padding
       "00 01 00 0a "           ;; :a
       "   00 02 00 01 02 "     ;; :b
       "   00 02 00 01 03 "     ;; :b
       "00 03 00 0c "           ;; :c
       "   00 04 00 08 "        ;; :d
       "      05 03 01 02 03 "  ;; :e
       "      02 01 04 "        ;; :b
       "00 06 00 02 aa aa"))    ;; :f
(def TLVS-TEST-1-BUF (parse-raw-msg TLVS-TEST-1-STR))


(def TLVS-TEST-2
  [[:b 17]
   [:e [18 19]]
   [:stop nil]])
(def TLVS-TEST-2-STR
  (str "00 00 "                 ;; padding
       "00 02 00 01 11 "        ;; :b
       "00 05 00 02 12 13 "     ;; :e
       "00 ff "                 ;; stop code
       "00 02 00 01 14"))       ;; :b (ignored)
(def TLVS-TEST-2-BUF (parse-raw-msg TLVS-TEST-2-STR))

(def TLVS-TEST-1-ASSOC-A-B
  [[:a [[:b 7]
        [:b 7]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]
   [:f {:bflag1 true
        :iflag2 1365
        :iflag3 2}]])

(def TLVS-TEST-1-INC-A-B
  [[:a [[:b 3]
        [:b 4]]]
   [:c [[:d [[:e [1 2 3]]
             [:b 4]]]]]
   [:f {:bflag1 true
        :iflag2 1365
        :iflag3 2}]])

(def TLVS-TEST-1-CONJ-E
  [[:a [[:b 2]
        [:b 3]]]
   [:c [[:d [[:e [1 2 3 7]]
             [:b 4]]]]]
   [:f {:bflag1 true
        :iflag2 1365
        :iflag3 2}]])

(def TLVS-CTX
  {:lookup TLVS-LOOKUP
   :tlv-tsize 2
   :tlv-lsize 2
   :readers readers
   :writers writers})

(deftest test-tlv-roundtrip
  (println "  test-tlv-roundtrip")
  (let [buf (.alloc js/Buffer 1500)
        wend ((writers :tlv) buf TLV-TEST-1 2 TLVS-CTX)
        res-buf (.slice buf 0 wend)
        res-msg ((readers :tlv) res-buf 2 wend TLVS-CTX)]
    ;;(js/console.log "orig BUF:" TLV-TEST-1-BUF)
    ;;(js/console.log "res  BUF:" res-buf)
    (is (= wend (-> res-msg meta :protocol/end)))
    (is (= 0 (.compare TLV-TEST-1-BUF res-buf)))
    (is (= TLV-TEST-1 res-msg))))

(deftest test-tlvs-roundtrip
  (println "  test-tlvs-roundtrip")
  (let [buf (.alloc js/Buffer 1500)
        wend ((writers :tlv-seq) buf TLVS-TEST-1 2 TLVS-CTX)
        res-buf (.slice buf 0 wend)
        res-msg ((readers :tlv-seq) res-buf 2 wend TLVS-CTX)]
    ;;(js/console.log "orig BUF:" TLVS-TEST-1-BUF)
    ;;(js/console.log "res  BUF:" res-buf)
    (is (= wend (-> res-msg meta :protocol/end)))
    (is (= 0 (.compare TLVS-TEST-1-BUF res-buf)))
    (is (= TLVS-TEST-1 res-msg))))

(deftest test-tlvs-stop
  (println "  test-tlvs-stop")
  (let [buf TLVS-TEST-2-BUF
        res-msg ((readers :tlv-seq) buf 2 (.-length buf) TLVS-CTX)]
    ;;(prn :res-msg res-msg)
    (is (-> res-msg meta :protocol/stop))
    (is (= TLVS-TEST-2 res-msg))))

(deftest test-get-in-tlv
  (println "  test-get-in-tlv")
  (println "    exists")
  (is (= [2 3] (tlvs/get-in-tlv* TLVS-TEST-1 [:a :b])))
  (is (= [2 3] (tlvs/get-in-tlv {:tlvs TLVS-TEST-1} [:a :b])))
  (is (= [4] (tlvs/get-in-tlv* TLVS-TEST-1 [:c :d :b])))
  (is (= [4] (tlvs/get-in-tlv {:foo :bar :tlvs TLVS-TEST-1} [:c :d :b])))
  (println "    not exists")
  (is (= nil (tlvs/get-in-tlv* TLVS-TEST-1 [:a :c])))
  (is (= nil (tlvs/get-in-tlv {:tlvs TLVS-TEST-1} [:a :c])))
  )

(deftest test-assoc-in-tlv
  (println "  test-assoc-in-tlv")
  (println "    exists")
  (is (= TLVS-TEST-1-ASSOC-A-B
         (tlvs/assoc-in-tlv* TLVS-TEST-1 [:a :b] 7)))
  (is (= {:tlvs TLVS-TEST-1-ASSOC-A-B}
         (tlvs/assoc-in-tlv {:tlvs TLVS-TEST-1} [:a :b] 7)))

  (println "    not exists")
  (is (= TLVS-TEST-1
         (tlvs/assoc-in-tlv* TLVS-TEST-1 [:a :c] 7)))
  (is (= {:tlvs TLVS-TEST-1}
         (tlvs/assoc-in-tlv {:tlvs TLVS-TEST-1} [:a :c] 7))))

(deftest test-update-in-tlv
  (println "  test-update-in-tlv")
  (is (= TLVS-TEST-1-INC-A-B
         (tlvs/update-in-tlv* TLVS-TEST-1 [:a :b] inc)))
  (is (= {:tlvs TLVS-TEST-1-INC-A-B}
         (tlvs/update-in-tlv {:tlvs TLVS-TEST-1} [:a :b] inc)))
  (is (= TLVS-TEST-1-CONJ-E
         (tlvs/update-in-tlv* TLVS-TEST-1 [:c :d :e] conj 7)))
  (is (= {:tlvs TLVS-TEST-1-CONJ-E}
         (tlvs/update-in-tlv {:tlvs TLVS-TEST-1} [:c :d :e] conj 7))))

