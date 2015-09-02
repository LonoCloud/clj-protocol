(ns dhcp.core
  (:require [cljs.nodejs :as nodejs]
            [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RFC defined values (uppercased)

(def RECV-PORT 67)
(def SEND-PORT 68)

(def HEADER-LIST
;; bytes, type, name
  [[1    :int8   :op]
   [1    :int8   :htype]
   [1    :int8   :hlen]
   [1    :int8   :hops]
   [4    :int32  :xid]
   [2    :int16  :secs]
   [2    :int16  :flags]
   [4    :ipv4   :ciaddr]
   [4    :ipv4   :yiaddr]
   [4    :ipv4   :siaddr] ;; next server
   [4    :ipv4   :giaddr]
   [6    :mac    :chaddr]
   [10   :raw    :chaddr-extra]
   [64   :str    :sname]
   ;; Dupe of opt-bootfile but with zero padding
   [128  :str    :opt/bootfile] ;; :file
   [4    :raw    :cookie]])

(def HEADER-ENDS (reductions + (map first HEADER-LIST)))
(def HEADER-SIZE (last HEADER-ENDS))

(def HEADERS
  (map #(zipmap [:hstart :hend :htype :hname] %&)
       (cons 0 HEADER-ENDS)     ;; header start offsets
       HEADER-ENDS              ;; header end offsets
       (map second HEADER-LIST) ;; header types
       (map last HEADER-LIST))) ;; header names

(def HEADERS-FIXED {:htype  1
                    :hlen   6
                    :hops   0 ;; fixed until relay supported
                    :cookie [99 130 83 99]}) ;; 0x63825363

(def OPTS-PAD 0)
(def OPTS-END 255)
(def OPT-MAX-LEN 255)
(def OPTS-LIST {:list (into ;; code, type, name
                       [[53  :msg-type  :opt/msg-type] ;; Typically sent first
                        [1   :ipv4      :opt/netmask]
                        [3   :ipv4      :opt/router]
                        [6   :ipv4set   :opt/dns-servers]
                        [12  :raw       :opt/hostname]
                        [15  :raw       :opt/domainname]
                        [28  :ipv4      :opt/broadcast]
                        [43  :raw       :opt/vend-spec-info]
                        [50  :ipv4      :opt/addr-req]
                        [51  :int32     :opt/lease-time]
                        [54  :ipv4      :opt/dhcp-server-id]
                        [55  :raw       :opt/parm-list]
                        [58  :int32     :opt/renew-time]
                        [59  :int32     :opt/rebind-time]
                        [60  :raw       :opt/vend-cls-ident]
                        [67  :str       :opt/bootfile]
                        [97  :raw       :opt/guid]

                        ;; http://www.iana.org/assignments/bootp-dhcp-parameters/bootp-dhcp-parameters.xml
                        [175 :encap     :opt/etherboot]] ;; gPXE/iPXE

                       ;; RFC-3942 site-specific options (224-254)
                       (map (fn [n]
                              [n :raw (keyword (str "opt-site-" n))])
                            (range 224 (inc 254))))
                :client-only #{:opt/parm-list :opt/addr-req}})

(def OPTS-ETHERBOOT-LIST {:list ;; code, type, name
                          [[0x01  :int8  :eb-priority]
                           [0x08  :raw   :eb-yi-addr]
                           [0x51  :raw   :eb-scriptlet]
                           [0xb2  :int8  :eb-use-cached]
                           [0xbe  :str   :eb-username]
                           [0xbf  :str   :eb-password]]
                          :client-only #{}})

(defn olist->map [olist]
  {:list (:list olist)
   :types (merge (into {} (map (fn [[c t n]] [c t]) (:list olist)))
                 (into {} (map (fn [[c t n]] [n t]) (:list olist))))
   :names (into {} (map (fn [[c t n]] [n c]) (:list olist)))
   :codes (into {} (map (fn [[c t n]] [c n]) (:list olist)))
   :client-only (:client-only olist)})

(def OPTS-LOOKUP (olist->map OPTS-LIST))
(def OPTS-ETHERBOOT-LOOKUP (olist->map OPTS-ETHERBOOT-LIST))

(def MSG-TYPE-LIST [;; num,  message, resp
                    [1   :DISCOVER     :OFFER]
                    [2   :OFFER        nil]
                    [3   :REQUEST      :ACK]
                    [4   :DECLINE      nil]
                    [5   :ACK          nil]
                    [6   :NAK          nil]
                    [7   :RELEASE      :ACK]
                    [8   :INFORM       :ACK]])
(def MSG-TYPE-LOOKUP
  (merge (into {} (map (fn [[n m r]] [n m]) MSG-TYPE-LIST))
         (into {} (map (fn [[n m r]] [m n]) MSG-TYPE-LIST))))
(def MSG-TYPE-RESP-LOOKUP
  (into {} (map (fn [[n m r]] [m r]) MSG-TYPE-LIST)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General DHCP message reading/writing

(def readers
  {:raw   identity
   :str   #(apply str (map char (take-while pos? %1))) ;; strips 0 chars
   :int8  #(first %1)
   :int16 #(reduce (fn [a b] (+ b (bit-shift-left a 8))) (take 2 %1))
   :int32 #(reduce (fn [a b] (+ b (bit-shift-left a 8))) (take 4 %1))
   :ipv4  #(take 4 %1)
   :mac   #(take 6 %1)
   :msg-type #(get MSG-TYPE-LOOKUP (first %1))})

(def writers
  {:raw      identity
   :str      #(for [c %1 :when (not= 0 c)] (.charCodeAt c 0))
   :int8     #(list %1)
   :int16    #(for [s [8 0]] (bit-and 255 (bit-shift-right %1 s)))
   :int32    #(for [s [24 16 8 0]] (bit-and 255 (bit-shift-right %1 s)))
   :ipv4     identity
   :ipv4set  #(apply concat %1)
   :mac      identity
   :msg-type #(list (get MSG-TYPE-LOOKUP %1))})


(defn read-header [msg]
  (into {} (for [{:keys [hstart hend htype hname]} HEADERS]
             [hname ((readers htype) (drop hstart (take hend msg)))])))

(defn read-opts
  [opts-lookup msg]
  (loop [opts {} [code & msg] msg]
    (cond
     (not (seq msg)) opts
     (= code OPTS-END) opts
     (= code OPTS-PAD) (recur opts msg)
     :else (let [[olen & msg] msg
                 data (take olen msg)
                 rdata (drop olen msg)
                 val ((readers (get (:types opts-lookup) code :raw)) data)
                 opts (assoc opts (get (:codes opts-lookup) code code) val)]
             (recur opts rdata)))))

(defn read-message
  [msg]
  (merge
   (read-header msg)
   (read-opts OPTS-LOOKUP (drop HEADER-SIZE msg))))

(defn write-header [msg-map]
  (apply concat
         (for [{:keys [hstart hend htype hname]} HEADERS]
           (let [data ((writers htype) (get msg-map hname))]
             (assert (not= data '(nil)) (str "unset header field " hname))
             (concat
              data
              (take (- hend hstart (count data)) (repeat 0)))))))

(defn write-opts [{:keys [types names]} msg-map]
  (concat
   (apply concat
          (for [[oname oval] (select-keys msg-map (keys types))]
            (let [data ((writers (get types oname :raw)) oval)]
              (when (> (count data) 0)
                (concat [(get names oname oname) (count data)] data)))))
   [OPTS-END]))

(defn write-message [msg-map]
  (let [msg-map (merge msg-map HEADERS-FIXED)
        msg (concat (write-header msg-map)
                    (write-opts OPTS-LOOKUP msg-map))]
    (assert (every? number? msg))
    msg))

(defn default-response [msg-map srv-if]
  (assoc (select-keys msg-map [:xid :secs :chaddr])
    :op 2 ;; DHCP response
    :siaddr (:address srv-if)
    :opt/msg-type (get MSG-TYPE-RESP-LOOKUP (:opt/msg-type msg-map))
    :opt/netmask (:netmask srv-if)
    :opt/router (:address srv-if)
    :opt/lease-time (* 60 60 24) ;; default to 1 day
    :opt/dhcp-server-id (:address srv-if)
    :opt/broadcast (:broadcast srv-if)
    :opt/dns-servers #{(:address srv-if)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPv4 Address support functions

(defn ip->octet [s]
  (map int (string/split s #"\.")))

(defn octet->ip [o]
  (string/join "." o))

(defn mac->octet [s]
  (map #(int (str "0x" %1)) (string/split s #":")))

(defn int->hex [i]
  (let [h (js/Number.prototype.toString.call i 16)]
    (if (= 1 (.-length h)) (str "0" h) h)))

(defn octet->mac [o]
  (string/join ":" (map int->hex o)))

(defn ip->int [ip]
  (reduce (fn [a b] (+ b (bit-shift-left a 8))) (ip->octet ip)))

(defn int->ip [int]
  (octet->ip
   (for [s [24 16 8 0]] (bit-and 255 (bit-shift-right int s)))))

(defn first-ip [ip netmask]
  (octet->ip
   (map #(bit-and %1 %2) (ip->octet ip) (ip->octet netmask))))

(defn broadcast [ip netmask]
  (octet->ip
   (map #(bit-or %1 %2)
        (ip->octet ip)
        (map #(+ 256 (bit-not %1)) (ip->octet netmask)))))

(defn network-start-end [ip netmask & [usable?]]
  (let [start (ip->int (first-ip ip netmask))
        end (ip->int (broadcast ip netmask))]
    (if (and usable? (not= start end))
      [(int->ip (+ 1 start)) (int->ip end)] ;; exclude network and broadcast
      [(int->ip start) (int->ip (+ 1 end))])))

;; Return a sequence of addresses for an IP and netmask
(defn ip-seq [start end]
  (map int->ip (range (ip->int start) (+ 1 (ip->int end)))))
