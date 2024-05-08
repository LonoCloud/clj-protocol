;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.core
  "DHCP protocol definition."
  (:require [protocol.platform :as plat]
            [protocol.fields :as fields]
            [protocol.addrs :as addrs]
            [protocol.tlvs :as tlvs]
            [protocol.header :as header]))

(def MAX-BUF-SIZE 1500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DHCP spec defined

(def RECV-PORT 67)
(def SEND-PORT 68)

(def MSG-TYPE-LIST [;; num,  message, resp, broadcast
                    [1   :DISCOVER        :OFFER   true]
                    [2   :OFFER           nil      nil]
                    [3   :REQUEST         :ACK     true]
                    [4   :DECLINE         nil      nil] ;; We don't currently handle decline
                    [5   :ACK             nil      nil]
                    [6   :NAK             nil      nil]
                    [7   :RELEASE         :ACK     false]
                    [8   :INFORM          :ACK     false]
                    [9   :FORCERENEW      :REQUEST false] ;; server to client
                    [10  :LEASEQUERY      nil      false]
                    [11  :LEASEUNASSIGNED nil      false]
                    [12  :LEASEUNKNOWN    nil      false]
                    [13  :LEASEACTIVE     nil      false]])

(def MSG-TYPE-LOOKUP (fields/list->lookup MSG-TYPE-LIST [0 1] [1 0]))
(def MSG-TYPE-RESP-LOOKUP (fields/list->lookup MSG-TYPE-LIST [1 2]))
(def MSG-TYPE-BCAST-LOOKUP (fields/list->lookup MSG-TYPE-LIST [1 3]))

;; https://datatracker.ietf.org/doc/html/rfc3046
;; https://datatracker.ietf.org/doc/html/rfc3527
(def OPTS-RELAY-AGENT-LIST
  ;; code, name, type
  [[0x01  :circuit-id     :raw   ]
   [0x02  :remote-id      :raw   ]
   [0x05  :link-selection :ipv4  ]
   [0x06  :subscriber-id  :utf8  ]])
(def OPTS-RELAY-AGENT-LOOKUP (tlvs/tlv-list->lookup OPTS-RELAY-AGENT-LIST))

(def OPTS-ETHERBOOT-LIST
  ;; code, name, type
  [[0x01  :eb-priority    :uint8 ]
   [0x08  :eb-yi-addr     :raw   ]
   [0x51  :eb-scriptlet   :raw   ]
   [0xb2  :eb-use-cached  :uint8 ]
   [0xbe  :eb-username    :utf8  ]
   [0xbf  :eb-password    :utf8  ]])
(def OPTS-ETHERBOOT-LOOKUP (tlvs/tlv-list->lookup OPTS-ETHERBOOT-LIST))

;; https://www.iana.org/assignments/bootp-dhcp-parameters/bootp-dhcp-parameters.xhtml
;; 53/msg-type is typically sent first
(def OPTS-LIST
  ;; code,  name,                type          extra-context
  (into
    [[53  :opt/msg-type          :lookup       {:lookup-type :uint8
                                                :lookup MSG-TYPE-LOOKUP}]
     [1   :opt/netmask           :ipv4         nil]
     [3   :opt/router            :repeat       {:repeat-type :ipv4 :repeat-size 4}]
     [4   :opt/time-servers      :repeat       {:repeat-type :ipv4 :repeat-size 4}]
     [5   :opt/name-servers      :repeat       {:repeat-type :ipv4 :repeat-size 4}]
     [6   :opt/dns-servers       :repeat       {:repeat-type :ipv4 :repeat-size 4}]
     [12  :opt/hostname          :utf8         nil]
     [15  :opt/domainname        :utf8         nil]
     [28  :opt/mtu               :uint16       nil]
     [28  :opt/broadcast         :ipv4         nil]
     [41  :opt/nis-servers       :repeat       {:repeat-type :ipv4 :repeat-size 4}]
     [43  :opt/vend-spec-info    :raw          nil]
     [50  :opt/addr-req          :ipv4         nil]
     [51  :opt/lease-time        :uint32       nil]
     [54  :opt/dhcp-server-id    :ipv4         nil]
     [55  :opt/parm-list         :raw          nil]
     [57  :opt/max-msg-size      :uint16       nil]
     [58  :opt/renew-time        :uint32       nil]
     [59  :opt/rebind-time       :uint32       nil]
     [60  :opt/vendor-class-id   :raw          nil]
     [61  :opt/client-id         :raw          nil]
     [67  :opt/bootfile          :utf8         nil]
     [82  :opt/relay-agent-info  :tlv-map      {:lookup OPTS-RELAY-AGENT-LOOKUP}]
     [97  :opt/guid              :raw          nil]
     [175 :opt/etherboot         :tlv-map      {:lookup OPTS-ETHERBOOT-LOOKUP}]]

    (concat
      ;; RFC-3942 site-specific options (224-254)
      (map (fn [n]
             [n (keyword (str "opt-site-" n)) :raw])
           (range 224 (inc 254)))

      [[255 :opt/end             :tlv-stop    nil ]])))
(def OPTS-LOOKUP (tlvs/tlv-list->lookup OPTS-LIST))



;; https://datatracker.ietf.org/doc/html/rfc2131
(def DHCP-FLAGS [[:broadcast  :bool   1]
                 [:reserved   :int   15]])

(def DHCP-HEADER
;;  name,          type,      extra-context
  [[:op            :uint8     {:default 0}]
   [:htype         :uint8     {:default 1}]
   [:hlen          :uint8     {:default 6}]
   [:hops          :uint8     {:default 0}]
   [:xid           :uint32    {:default 0}]
   [:secs          :uint16    {:default 0}]
   [:flags         :bitfield  {:length 2 :default 0 :spec DHCP-FLAGS}]
   [:ciaddr        :ipv4      {:default "0.0.0.0"}]
   [:yiaddr        :ipv4      {:default "0.0.0.0"}]
   [:siaddr        :ipv4      {:default "0.0.0.0"}] ;; next server
   [:giaddr        :ipv4      {:default "0.0.0.0"}]
   [:chaddr        :mac       {:default "00:00:00:00:00:00"}]
   [:chaddr-extra  :raw       {:length 10 :default [0 0 0 0 0 0 0 0 0 0]}]
   [:sname         :utf8      {:length 64 :default ""}]
   [:bootfile      :utf8      {:length 128 :default ""}] ;; :file
   [:cookie        :raw       {:length 4 :default [99 130 83 99]}]
   [:options       :tlv-map   {:tlv-tsize 1
                               :tlv-lsize 1
                               :lookup OPTS-LOOKUP}]])

(def HEADERS-FIXED {:htype  1
                    :hlen   6
                    :hops   0 ;; fixed until relay supported
                    :cookie [99 130 83 99]}) ;; 0x63825363

(def DHCP-DEFAULTS
  (into {} (for [[fname ftype {:keys [default]}] DHCP-HEADER
                 :when default]
             [fname default])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General DHCP message reading/writing

(def ^:private readers (merge fields/readers-BE addrs/readers tlvs/readers))
(def ^:private writers (merge fields/writers-BE addrs/writers tlvs/writers))

(defn read-dhcp
  "Read/decode a DHCP payload from `buf`"
  [buf & [spec]]
  ;; Merge options up into the top level map
  (let [msg-map (header/read-header-full buf 0 {:readers readers
                                                :spec (or spec DHCP-HEADER)})
        options (:options msg-map)]
    (dissoc (merge msg-map options)
            :options
            :opt/end)))

(defn write-dhcp
  "Write/encode an DHCP payload into an allocated buffer using
  `msg-map`. Returns the allocated buffer sliced to the size written."
  [msg-map & [spec]]
  ;; Move options down into :options keys
  (let [options (into {:opt/end 0}
                      (for [fname (map second OPTS-LIST)
                            :when (contains? msg-map fname)]
                        [fname (get msg-map fname)]))
        msg-map (merge msg-map HEADERS-FIXED {:options options})
        buf (plat/buf-alloc MAX-BUF-SIZE)]
    (header/write-header-full buf msg-map 0 {:writers writers
                                             :spec (or spec DHCP-HEADER)})))


(defn default-response
  "Takes a msg-map containing a `:opt/msg-type` and based on the
  msg-type and network info from `srv-if`, returns a populated
  msg-map with default values."
  [msg-map srv-if]
  (let [msg-type (:opt/msg-type msg-map)]
    (merge
      DHCP-DEFAULTS
      (select-keys msg-map [:xid :secs :chaddr])
      {:op                 2 ;; DHCP response
       :flags              {:broadcast (get MSG-TYPE-BCAST-LOOKUP msg-type)
                            :reserved 0}
       :opt/msg-type       (get MSG-TYPE-RESP-LOOKUP msg-type)
       :opt/lease-time     (* 60 60 24) ;; default to 1 day
       :siaddr             (:address srv-if)
       :opt/netmask        (:netmask srv-if)
       :opt/router         [(:address srv-if)]
       :opt/dhcp-server-id (:address srv-if)
       :opt/broadcast      (:broadcast srv-if)
       :opt/dns-servers    [(:address srv-if)]}
      HEADERS-FIXED)))

