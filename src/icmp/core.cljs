(ns icmp.core
  (:require [protocol.fields :as fields]
            [protocol.addrs :as addrs]
            [protocol.header :as header]))

(def MAX-BUF-SIZE 1500)

(def MSG-TYPE-LIST
  [[ 0  :echo-reply               ]   ;; ping response
   [ 3  :dest-unreachable         ]
   [ 4  :source-quench            ]
   [ 5  :redirect                 ]
   [ 8  :echo-request             ] ;; ping
   [ 9  :router-advertisement     ]
   [10  :router-solicitation      ]
   [11  :time-exceeded            ]
   [12  :bad-ip-header            ]
   [13  :timestamp                ]
   [14  :timestamp-reply          ]
   [15  :information-request      ]
   [16  :information-reply        ]
   [17  :address-mask-request     ]
   [42  :extended-echo-request    ]
   [43  :extended-echo-reply      ]
   [18  :address-mask-reply       ]])
(def MSG-TYPE-LOOKUP (fields/list->lookup MSG-TYPE-LIST [0 1] [1 0]))

(def MSG-TYPE-HEADERS
  ;; msg-type           name,          type,     extra-context
  {:echo-request      [[:id            :uint16   {:default 0}]
                       [:seq-num       :uint16   {:default 0}]
                       [:payload       :raw      {:length :*}]]
   :echo-reply        [[:id            :uint16   {:default 0}]
                       [:seq-num       :uint16   {:default 0}]
                       [:payload       :raw      {:length :*}]]
   :dest-unreachable  [[:unused        :uint32   {:default 0}]
                       [:orig-packet   :raw      {:length :*}]]
   :redirect          [[:gw-addr       :ipv4     {:default ""}]
                       [:orig-packet   :raw      {:length :*}]]})
(def MSG-TYPE-MAP
  (into {} (for [[k v] MSG-TYPE-HEADERS] [k {:choice-type :header :spec v}])))

(def ICMP-HEADER
;;  name,        type,     extra-context
  [[:type        :lookup   {:lookup-type :uint8
                            :lookup MSG-TYPE-LOOKUP}]
   [:code        :uint8    {:default 0}]
   [:checksum    :uint16   {:default 0}]
   [:data        :choice   {:choice-path [:type]
                            :choices MSG-TYPE-MAP}]])

(def ^:private readers (merge fields/readers-BE addrs/readers header/readers))
(def ^:private writers (merge fields/writers-BE addrs/writers header/writers))

(defn read-icmp
  "Read/decode an ICMP payload from `buf`"
  [buf]
  (header/read-header-full buf 0 {:readers readers
                                  :spec ICMP-HEADER}))

(defn write-icmp
  "Write/encode an ICMP payload into an allocated js/Buffer using
  `msg-map`. Returns the allocated buffer sliced to the size written."
  [msg-map]
  (let [buf (.alloc js/Buffer MAX-BUF-SIZE)]
    (header/write-header-full buf msg-map 0 {:writers writers
                                             :spec ICMP-HEADER})))

