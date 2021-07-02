(ns protocol.fields-test
  (:require [cljs.test :refer-macros [deftest is]]
            [protocol.fields :as fields]))

(deftest test-net-addr-functions
  (println "  test-net-addr-functions")
  (is (= 16909060      (fields/octet->int [1 2 3 4])))
  (is (= [1 2 3 4]     (fields/int->octet 16909060 4)))
  (is (= [0 0 1 2 3 4] (fields/int->octet 16909060 6)))
  (is (= 1108152157446 (fields/octet->int [1 2 3 4 5 6])))
  (is (= [1 2 3 4 5 6] (fields/int->octet 1108152157446 6)))
  )

;;;;;

(deftest test-readers
  (println "  test-readers")
  (let [buf (.from js/Buffer (clj->js [65 66 67 68 69 70 71 72 73 74]))]
    (doseq [[t v] {:str "CDEF"
                   :uint8 67
                   :uint16 17220
                   :uint32 1128547654
                   :uint64 (js/BigInt "4847075267103443274")
                   :ipv4 [67 68 69 70]
                   :mac [67 68 69 70 71 72]}]
      (println "    reader" t)
      (let [res ((fields/readers t) buf 2 6)]
        (is res)
        (is (= v res))))))

(deftest test-writers
  (println "  test-writers")
  (doseq [[t [v1 v2 v3]]
          {:str    ["CDEF"              4 [0 0 67 68 69 70  0  0  0  0]]
           :uint8  [67                  1 [0 0 67  0  0  0  0  0  0  0]]
           :uint16 [17220               2 [0 0 67 68  0  0  0  0  0  0]]
           :uint32 [1128547654          4 [0 0 67 68 69 70  0  0  0  0]]
           :uint64 [(js/BigInt "4847075267103443274")
                                        8 [0 0 67 68 69 70 71 72 73 74]]
           :ipv4   [[67 68 69 70]       4 [0 0 67 68 69 70  0  0  0  0]]
           :mac    [[67 68 69 70 71 72] 6 [0 0 67 68 69 70 71 72  0  0]]}]
    (println "    writer" t v1 v2 v3)
    (let [buf (.alloc js/Buffer 10)
          sz ((fields/writers t) buf v1 2 6)
          octs (vec (.slice buf 0))]
      (is (> sz 0))
      (is (= v3 octs)))))

(deftest test-string-readers-writers
  (println "  test-string-readers-writers")
  (let [arr [0 0 65 66 67 68 0 0]
        buf (.from js/Buffer (clj->js arr))]
    (println "    readers")
    (is (= "ABCD" ((fields/readers :str) buf 2 6)))
    ;; Test that zero bytes are ignored
    (is (= "ABCD" ((fields/readers :str) buf 2 8))))
  (let [arr [0 0 69 70 71 72 0 0]]
    (println "    writers")
    (let [buf (.alloc js/Buffer 8)]
      ((fields/writers :str) buf "EFGH" 2 6)
      (is (= 0 (.compare buf (.from js/Buffer (clj->js arr)))))
      (is (= "EFGH" ((fields/readers :str) buf 2 8))))))

