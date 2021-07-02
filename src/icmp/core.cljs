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
  ;; name,          type,         length,    extra-context
  {:echo-request
   [[:id            :uint16         2        {:default 0}]
    [:seq-num       :uint16         2        {:default 0}]
    [:payload       :raw            :*       {}]]

   :echo-reply
   [[:id            :uint16         2        {:default 0}]
    [:seq-num       :uint16         2        {:default 0}]
    [:payload       :raw            :*       {}]]

   :destination-unreachable
   [[:unused        :uint32         4        {:default 0}]
    [:orig-packet   :raw            :*       {}]]

   :redirect
   [[:gw-addr       :ipv4           4        {:default ""}]
    [:orig-packet   :raw            :*       {}]]})

(def ICMP-HEADER
;;  name,          type,         length,    extra-context
  [[:type          :msg-type       1        {:default 0}]
   [:code          :uint8          1        {:default 0}]
   [:checksum      :uint16         2        {:default 0}]
   [:data          :icmp-data      :*       {:type-headers
                                             ICMP-TYPE-HEADERS} ]])

(defn read-icmp-data [buf start end ctx]
  (let [msg-type (get-in ctx [:msg-map :type])
        type-header (get-in ctx [:type-headers msg-type])]
    (header/read-header buf start end (assoc ctx :spec type-header))))

(defn write-icmp-data [buf msg-map start ctx]
  (let [msg-type (get-in ctx [:msg-map :type])
        type-header (get-in ctx [:type-headers msg-type])
        ctx (assoc ctx :spec type-header)]
    (header/write-header buf msg-map start ctx)))

(set! *warn-on-infer* false)

(def readers
  (merge
    fields/readers-BE
    addrs/readers
    {:msg-type  #(let [tnum (.readUInt8 %1 %2)
                       typ (get MSG-TYPE-LOOKUP tnum)]
                  (assert typ (str "Unknown ICMP message type " tnum))
                  typ)
     :icmp-data read-icmp-data}))

(def writers
  (merge
    fields/writers-BE
    addrs/writers
    {:msg-type   #(.writeUInt8 %1 (get MSG-TYPE-LOOKUP %2) %3)
     :icmp-data  write-icmp-data}))

(set! *warn-on-infer* true)

(defn read-icmp [buf]
  (header/read-header-full buf 0 (.-length buf)
                           {:readers readers :spec ICMP-HEADER}))

(defn write-icmp [msg-map]
  (let [buf (.alloc js/Buffer MAX-BUF-SIZE)]
    (header/write-header-full buf msg-map 0
                              {:writers writers :spec ICMP-HEADER})))

