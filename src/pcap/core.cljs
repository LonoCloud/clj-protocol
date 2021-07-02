(ns pcap.core
  (:require [protocol.addrs :as addrs]
            [protocol.fields :as fields]
            [protocol.header :as header]))

(def fs (js/require "fs"))

;; https://tools.ietf.org/id/draft-gharris-opsawg-pcap-00.html
;; https://github.com/pcapng/pcapng

(def ARP-V4-HEADER
  ;;name,       type,         length,   extra-context
  [[:htype      :uint16       2         {:default 0}]
   [:ptype      :uint16       2         {:default 0}]
   [:hlen       :uint8        1         {:default 0}]
   [:plen       :uint8        1         {:default 0}]
   [:oper       :uint16       2         {:default 0}]
   [:sha        :mac          6         {:default 0}]
   [:spa        :ipv4         4         {:default 0}]
   [:tha        :mac          6         {:default 0}]
   [:tpa        :ipv4         4         {:default 0}]])

(def IP-V4-HEADER
  ;;name,       type,         length,   default,  extra-context
  [[:ver-ihl    :bitfield     1         {:default 0
                                         :spec [[:version  :int  4]
                                                [:ihl      :int  4]]}]
   [:tos        :uint8        1         {:default 0}]
   [:length     :uint16       2         {:default 0}]
   [:id         :uint16       2         {:default 0}]
   [:flags-frag :uint16       2         {:default 0}]
   [:ttl        :uint8        1         {:default 0}]
   [:protocol   :uint8        1         {:default 0}]
   [:checksum   :uint16       2         {:default 0}]
   [:src-addr   :ipv4         4         {:default 0}]
   [:dst-addr   :ipv4         4         {:default 0}]
   [:payload    :raw          :*        {:default 0}]])

(def ETHERTYPE-MAP
  {0x0806 {:choice-type :header :spec ARP-V4-HEADER}
   0x0800 {:choice-type :header :spec IP-V4-HEADER}})

(def ETHERNET-HEADER
  ;;name,       type,         length,   default,  extra-context
  [[:dst-mac    :mac          6         {:default ""}]
   [:src-mac    :mac          6         {:default ""}]
   [:ethertype  :uint16       2         {:default 0}]
   [:payload    :choice       :*        {:choice-on :ethertype
                                         :choices ETHERTYPE-MAP}]])

(def packet-readers (merge fields/readers-BE addrs/readers header/readers))

(def RECORD-HEADER
  ;;name,       type,       length,   default,  extra-context
  [[:ts-secs    :uint32      4         {:default 0}]
   [:ts-part    :uint32      4         {:default 0}]
   [:cap-len    :uint32      4         {:default 0}]
   [:orig-len   :uint32      4         {:default 0}]
   [:packet     :header      :cap-len  {:spec ETHERNET-HEADER
                                        :readers packet-readers}]])

(def LINKTYPE-BITFIELD
  ;;name,   type,  length
  [[:fcs    :int    3]
   [:f      :bool   1]
   [:type   :int   28]])

(def PCAP-HEADER
  ;;name,       type,        length,  default,  extra-context
  [[:magic      :uint32      4        {:default 0}]
   [:ver-major  :uint16      2        {:default 0}]
   [:ver-minor  :uint16      2        {:default 0}]
   [:reserved1  :uint32      4        {:default 0}]
   [:reserved2  :uint32      4        {:default 0}]
   [:snaplen    :uint32      4        {:default 0}]
   [:fcs-type   :bitfield    4        {:spec LINKTYPE-BITFIELD}]
   [:records    :loop        :*       {:loop-type :header
                                       :spec RECORD-HEADER}]])

(def trace-readers (merge fields/readers-LE header/readers))

(defn parse-file [path]
  (let [buf (.readFileSync fs path)]
    (header/read-header buf 0 (.-length buf)
                        {:readers trace-readers
                         :spec PCAP-HEADER})))

(defn main [path]
  (let [trace (parse-file path)]
    (println "File header:")
    (println (dissoc trace :records))
    (println "Records:")
    (doseq [record (:records trace)] (prn record))))
