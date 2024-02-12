;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns pcap.live
  (:require [clojure.pprint :refer [pprint]]
            [protocol.fields :as fields]
            [protocol.header :as header]
            [protocol.addrs :as addrs]
            [pcap.core :as clj-pcap]
            ["pcap$default" :as js-pcap]
            ["fs" :as fs]))

(def pcap-readers (merge fields/readers-LE header/readers))

(defn read-pcap-header-full
  "Read a PCAP header and (confusingly also) PCAP records from a
  js/Buffer."
  [buf]
  (let [ctx {:readers pcap-readers
             :spec clj-pcap/PCAP-HEADER}]
    (header/read-header-full buf 0 ctx)))

(defn read-pcap-record-full
  "Read a PCAP record from a js/Buffer."
  [buf]
  (let [ctx {:readers pcap-readers
             :spec clj-pcap/RECORD-HEADER}]
    (header/read-header-full buf 0 ctx)))

(defn read-ethernet-full
  "Read an Ethernet header and payload a js/Buffer."
  [buf]
  (let [ctx {:readers (merge fields/readers-BE header/readers addrs/readers)
             :spec clj-pcap/ETHERNET-HEADER}]
    (header/read-header-full buf 0 ctx)))

(defn pcap-records
  "Transducer that transforms a filename into pcap records. Each record
  is a map with `:pcap-header` containing a decoded PCAP-HEADER and
  the `:pcap-record` key containing a decoded PCAP-RECORD from the
  `clj-protocol.pcap` namespace. A `:frame` key is added for the index
  of the record in the pcap file."
  [xf]
  (fn [result input]
    (when input
      (let [pcap (-> input fs/readFileSync read-pcap-header-full)
            pcap-header (dissoc pcap :records)
            pcap-recs (map-indexed #(merge {:frame (inc %1)} %2) (:records pcap))]
        (doseq [rec pcap-recs]
          (xf result {:pcap-header pcap-header :pcap-record rec}))))))

(defn pcap-records-live
  "Transducer that transforms a map of interface name and capture filter
  string into Ethernet link-layer frames. Returns map with only a key
  of `:pcap-record`."
  [xf]
  (fn [result input]
    (when input
      (let [{:keys [iface filter]} input
            session (js-pcap/createSession iface #js{:filter filter})]
        (doto session
          (.on "packet" (fn [packet]
                          (let [hdr ^js/Buffer (.-header packet)
                                rec (read-pcap-record-full hdr)
                                buf (.slice ^js/Buffer (.-buf packet) 0 (:cap-len rec))]
                            (xf result {:pcap-record (merge rec {:packet (read-ethernet-full buf)})})))))))))

(defn rec->eth
  "Transducer to decode a PCAP record to Ethernet."
  [xf]
  (fn [result {:keys [pcap-header pcap-record] :as input}]
    (xf result
        (-> input
            (merge {:eth (:packet pcap-record)})
            (dissoc :pcap-record)))))

(defn eth->ip
  "Transducer to decode an Ethernet frame to IP."
  [xf]
  (fn [result input]
    (xf result
        (let [ethertype (or (get-in input [:eth :ethertype])  ;; LINKTYPE_ETHERNET
                            (get-in input [:eth :protocol]))] ;; LINKTYPE_LINUX_SLL
          (if (= ethertype 0x0800)
            (let [ip (get-in input [:eth :payload])]
              (-> input
                  (merge {:ip ip})
                  (dissoc :pcap-header)
                  (update :eth dissoc :payload))))))))

(def UDP-V4-HEADER                         ;; TODO: move into pcap.core
  [[:src-port :uint16 {:default 0}]
   [:dst-port :uint16 {:default 0}]
   [:length   :uint16 {:default 0}]
   [:checksum :uint16 {:default 0}]
   [:payload  :buf    {:length :length}]]) ;; TODO: length from start of buffer

(defn read-udp
  [buf]
  (header/read-header-full buf 0 {:readers (merge fields/readers-BE header/readers)
                                  :spec UDP-V4-HEADER}))

(defn ip->udp
  "Transducer to decode an IP packet to UDP."
  [xf]
  (fn [result input]
    (xf result
        (if (= (get-in input [:ip :protocol]) 0x11)
          (let [udp (read-udp (.from js/Buffer (clj->js (get-in input [:ip :payload]))))]
            (-> input
                (merge {:udp udp})
                (update :ip dissoc :payload)))
          input))))

(def decode-all
  "Decode as many typical protocols we know about."
  (comp
   rec->eth
   eth->ip
   ip->udp))
