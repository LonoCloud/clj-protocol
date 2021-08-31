;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns protocol.fields-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is]]
               :clj  [clojure.test :refer [deftest is]])
            [protocol.platform :as plat]
            [protocol.util :as util]
            [protocol.fields :as fields]))

(deftest test-field-functions
  (println "  test-field-functions")
  (is (= 16909060      (fields/octet->int [1 2 3 4])))
  (is (= [1 2 3 4]     (fields/int->octet 16909060 4)))
  (is (= [0 0 1 2 3 4] (fields/int->octet 16909060 6)))
  (is (= 1108152157446 (fields/octet->int [1 2 3 4 5 6])))
  (is (= [1 2 3 4 5 6] (fields/int->octet 1108152157446 6))))

;;;;;

(def freaders fields/readers-BE)
(def fwriters fields/writers-BE)

(deftest test-string-readers-writers
  (println "  test-string-readers-writers")
  (let [arr [0 0 65 66 67 68 0 0]
        buf (plat/buf-from arr)]
    (println "    readers")
    (is (= [6 "ABCD"] ((freaders :utf8) buf 2 {:length 4})))
    ;; Test that zero bytes are ignored
    (is (= [6 "ABCD"] ((freaders :utf8) buf 2 {:length 4}))))
  (let [arr [0 0 69 70 71 72 0 0]]
    (println "    writers")
    (let [buf (plat/buf-alloc 8)]
      ((fwriters :utf8) buf "EFGH" 2 {:length 4})
      ;;(println (util/pr-buf buf {:prefix "buf: "}))
      ;;(println (util/pr-buf (plat/buf-from arr) {:prefix "arr: "}))
      (is (= 0 (plat/buf-cmp buf (plat/buf-from arr))))
      (is (= [6 "EFGH"] ((freaders :utf8) buf 2 {:length 4}))))))

(deftest test-readers
  (println "  test-readers")
  (let [buf (plat/buf-from [65 66 67 68 69 70 71 72 73 74])
        bit-spec [[:a :int 10] [:b :bool 1] [:c :bool 3] [:d :int 18]]]
    (doseq [[t l v] [[:utf8      4  "CDEF"]
                     [:uint8     1  67]
                     [:uint16    2  17220]
                     [:uint32    4  1128547654]
                     [:uint64    8  (plat/big-int "4847075267103443274")]
                     [:repeat    4  [67 68 69 70]]
                     [:loop      4  [67 68 69 70]]
                     [:bitfield  4  {:a 269, :b false, :c true, :d 17734}]]]
      (println "    reader" t)
      (let [[end res] ((freaders t) buf 2 {:readers freaders
                                           :length l
                                           :spec bit-spec
                                           :repeat-type :uint8
                                           :repeat-size 1
                                           :loop-type :uint8})]
        (is (= end (+ 2 l)))
        (is res)
        (is (= v res))))))

(deftest test-writers
  (println "  test-writers")
  (let [bit-spec [[:a :int 10] [:b :bool 1] [:c :bool 3] [:d :int 18]]]
    (doseq [[t [v l r]]
            [[:utf8      ["CDEF"                 4 [0 0 67 68 69 70  0  0  0  0]]]
             [:utf8      ["CDEF"               nil [0 0 67 68 69 70  0  0  0  0]]]
             [:uint8     [67                     1 [0 0 67  0  0  0  0  0  0  0]]]
             [:uint16    [17220                  2 [0 0 67 68  0  0  0  0  0  0]]]
             [:uint32    [1128547654             4 [0 0 67 68 69 70  0  0  0  0]]]
             [:uint64    [(plat/big-int "4847075267103443274")
                          8 [0 0 67 68 69 70 71 72 73 74]]]
             [:repeat    [[67 68 69 70]          4 [0 0 67 68 69 70  0  0  0  0]]]
             [:loop      [[67 68 69 70]          4 [0 0 67 68 69 70  0  0  0  0]]]
             [:bitfield  [{:a 269, :b false, :c true, :d 17734}
                          4 [0 0 67 68 69 70  0  0  0  0]]]]]
      (println "    writer" t v l r)
      (let [buf (plat/buf-alloc 10)
            sz ((fwriters t) buf v 2 {:writers fwriters
                                      :length l
                                      :spec bit-spec
                                      :repeat-type :uint8
                                      :repeat-size 1
                                      :loop-type :uint8})
            octs (plat/buf->vec buf 0)]
        (is (> sz 0))
        (is (= r octs))))))

(deftest test-choice
  (println "  test-choice")
  (let [base-ctx {:readers freaders
                  :writers fwriters
                  :choice-path [:a]
                  :choices {1 {:choice-type :uint8}
                            2 {:choice-type :uint16}}}]
    (println "    reader")
    (let [reader (freaders :choice)
          buf (plat/buf-from [65 66 67 68 69 70])
          [end1 res1] (reader buf 2 (assoc base-ctx :msg-map {:a 1}))
          [end2 res2] (reader buf 2 (assoc base-ctx :msg-map {:a 2}))]
      (is (= 67 res1))
      (is (= 17220 res2))
      (is (thrown? #?(:cljs js/Error :clj AssertionError)
                   (reader buf 2 (assoc base-ctx :msg-map {}))))
      (is (thrown? #?(:cljs js/Error :clj AssertionError)
                   (reader buf 2 (assoc base-ctx :msg-map {:a 3})))))

    (println "    writer")
    (let [writer (fwriters :choice)
          buf1 (plat/buf-alloc 6)
          buf2 (plat/buf-alloc 6)
          sz1 (writer buf1 67    2 (assoc base-ctx :msg-map {:a 1}))
          sz2 (writer buf2 17220 2 (assoc base-ctx :msg-map {:a 2}))]
      (is (= [0 0 67  0 0 0] (plat/buf->vec buf1 0)))
      (is (= [0 0 67 68 0 0] (plat/buf->vec buf2 0)))
      )))

