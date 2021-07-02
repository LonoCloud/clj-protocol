(ns pcap.core
  (:require [protocol.addrs :as addrs]
            [protocol.fields :as fields]
            [protocol.header :as header]))

(def fs (js/require "fs"))

;; https://tools.ietf.org/id/draft-gharris-opsawg-pcap-00.html
;; https://github.com/pcapng/pcapng

(def ARP-V4-HEADER
  ;;name,       type,         length,   default,  extra-context
  [[:htype      :uint16       2         0]
   [:ptype      :uint16       2         0]
   [:hlen       :uint8        1         0]
   [:plen       :uint8        1         0]
   [:oper       :uint16       2         0]
   [:sha        :mac          6         0]
   [:spa        :ipv4         4         0]
   [:tha        :mac          6         0]
   [:tpa        :ipv4         4         0]])

(def IP-V4-HEADER
  ;;name,       type,         length,   default,  extra-context
  [[:ver-ihl    :bitfield     1         0         {:spec [[:version  :int  4]
                                                          [:ihl      :int  4]]}]
   [:tos        :uint8        1         0         nil]
   [:length     :uint16       2         0         nil]
   [:id         :uint16       2         0         nil]
   [:flags-frag :uint16       2         0         nil]
   [:ttl        :uint8        1         0         nil]
   [:protocol   :uint8        1         0         nil]
   [:checksum   :uint16       2         0         nil]
   [:src-addr   :ipv4         4         0         nil]
   [:dst-addr   :ipv4         4         0         nil]
   [:payload    :raw          :*        0         nil]])

(def ETHERTYPE-MAP
  {0x0806 {:choice-type :header :spec ARP-V4-HEADER}
   0x0800 {:choice-type :header :spec IP-V4-HEADER}})

(def ETHERNET-HEADER
  ;;name,       type,         length,   default,  extra-context
  [[:dst-mac    :mac          6         ""        nil]
   [:src-mac    :mac          6         ""        nil]
   [:ethertype  :uint16       2         0         nil]
   [:payload    :choice       :*        nil       {:choice-on :ethertype
                                                   :choices ETHERTYPE-MAP}]])

(def packet-readers
  (merge fields/readers-BE
         addrs/readers
         header/readers))

(def RECORD-HEADER
  ;;name,       type,       length,   default,  extra-context
  [[:ts-secs    :uint32      4         0         nil]
   [:ts-part    :uint32      4         0         nil]
   [:cap-len    :uint32      4         0         nil]
   [:orig-len   :uint32      4         0         nil]
   [:packet     :header      :cap-len  0         {:spec ETHERNET-HEADER
                                                  :readers packet-readers}]])

(def LINKTYPE-BITFIELD
  ;;name,   type,  length
  [[:fcs    :int    3]
   [:f      :bool   1]
   [:type   :int   28]])

(def PCAP-HEADER
  ;;name,       type,        length,  default,  extra-context
  [[:magic      :uint32      4        0         nil]
   [:ver-major  :uint16      2        0         nil]
   [:ver-minor  :uint16      2        0         nil]
   [:reserved1  :uint32      4        0         nil]
   [:reserved2  :uint32      4        0         nil]
   [:snaplen    :uint32      4        0         nil]
   [:fcs-type   :bitfield    4        nil       {:spec LINKTYPE-BITFIELD}]
   [:records    :loop        :*       nil       {:loop-type :header
                                                 :spec RECORD-HEADER}]])

(def readers
  (merge fields/readers-LE
         header/readers))

(defn parse-file [path]
  (let [buf (.readFileSync fs path)]
    (header/read-header buf 0 (.-length buf)
                        {:readers readers 
                         :spec PCAP-HEADER})))

(defn main [path]
  (let [trace (parse-file path)]
    (println "File header:")
    (println (dissoc trace :records))
    (println "Records:")
    (doseq [record (:records trace)] (prn record))))
