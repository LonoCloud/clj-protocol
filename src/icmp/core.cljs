(ns icmp.core
  (:require [protocol.fields :as fields]
            [protocol.addrs :as addrs]
            [protocol.header :as header]))

(def MAX-BUF-SIZE 1500)

(def MSG-TYPE-LIST
  [[ 0  :echo-reply               ]   ;; ping response
   [ 3  :destination-unreachable  ]
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
(def MSG-TYPE-LOOKUP
  (merge (into {} (map (fn [[c n]] [c n]) MSG-TYPE-LIST))
         (into {} (map (fn [[c n]] [n c]) MSG-TYPE-LIST))))

(def ICMP-TYPE-HEADERS
  {:echo-request
   ;;  name,          type,           length,  default
   [[:id            :uint16         2        0]
    [:seq-num       :uint16         2        0]
    [:payload       :raw            :*       nil]]

   :echo-reply
   [[:id            :uint16         2        0]
    [:seq-num       :uint16         2        0]
    [:payload       :raw            :*       nil]]

   :destination-unreachable
   [[:unused        :uint32         4        0]
    [:orig-packet   :raw            :*       nil]]

   :redirect
   [[:gw-addr       :ipv4           4        ""]
    [:orig-packet   :raw            :*       nil]]})

(def ICMP-HEADER
;;  name,          type,           length,  default,  extra-context
  [[:type          :msg-type       1        0]
   [:code          :uint8          1        0]
   [:checksum      :uint16         2        0]
   [:data          :icmp-data      :*       nil       {:type-headers
                                                       ICMP-TYPE-HEADERS} ]])

(defn read-icmp-data [buf start end ctx]
  (let [msg-type (get-in ctx [:msg-map :type])
        type-header (get-in ctx [:type-headers msg-type])]
    (header/read-header buf start end (assoc ctx :spec type-header))))

(defn write-icmp-data [buf msg-map start ctx]
  (let [msg-type (get-in ctx [:msg-map :type])
        type-header (get-in ctx [:type-headers msg-type])
        ctx (assoc ctx :spec type-header)]
    ;; Use composable version that returns length
    (header/write-header* buf msg-map start ctx)))

(defn read-icmp* [buf start end ctx]
  (header/read-header buf start end ctx))

(defn write-icmp* [buf msg-map start ctx]
  (let [buf (if buf buf (.alloc js/Buffer MAX-BUF-SIZE))]
    (header/write-header buf msg-map start ctx)))


(set! *warn-on-infer* false)

(def readers
  (merge
    fields/readers
    addrs/readers
    {:msg-type  #(let [tnum (.readUInt8 %1 %2)
                       typ (get MSG-TYPE-LOOKUP tnum)]
                  (assert typ (str "Unknown ICMP message type " tnum))
                  typ)
     :icmp-data read-icmp-data}))

(def writers
  (merge
    fields/writers
    addrs/writers
    {:msg-type   #(.writeUInt8 %1 (get MSG-TYPE-LOOKUP %2) %3)
     :icmp-data  write-icmp-data}))

(set! *warn-on-infer* true)

(defn read-icmp [buf]
  (read-icmp* buf 0 (.-length buf) {:readers readers :spec ICMP-HEADER}))

(defn write-icmp [msg-map]
  (write-icmp* nil msg-map 0 {:writers writers :spec ICMP-HEADER}))

