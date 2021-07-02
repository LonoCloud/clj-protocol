(ns protocol.socket)

(def ffi (js/require "@saleae/ffi"))
(def ref (js/require "ref"))

(def ETH-HDR-LEN 14)
;; Assumes no IP options (ihl = 5)
(def IPV4-HDR-LEN 20)
(def UDP-HDR-LEN 8)
(def UDP-PAYLOAD-OFF (+ ETH-HDR-LEN IPV4-HDR-LEN UDP-HDR-LEN))

;; domain or family
(def DOMAINS
  {:AF_UNIX        1
   :AF_INET        2
   :AF_PACKET     17})

(def TYPES
  {:SOCK_STREAM    1
   :SOCK_DGRAM     2
   :SOCK_RAW       3})

;; Protocols / Levels
(def PROTOCOLS
  ;; /usr/include/linux/in.h
  {:IPPROTO_IP     0
   :SOL_IP         0 ;; Same as above but deprecated
   :SOL_SOCKET     1
   :IPPROTO_ICMP   1
   :IPPROTO_TCP    6
   :IPPROTO_UDP   17})

(def OPTIONS*
  ;; /usr/include/asm-generic/socket.h
  {:SOL_SOCKET {:SO_REUSEADDR      2
                :SO_DONTROUTE      5
                :SO_BROADCAST      6
                :SO_RCVBUF         8
                :SO_REUSEPORT     15
                :SO_BINDTODEVICE  25}
   ;; /usr/include/linux/in.h
   :IPPROTO_IP {:IP_TOS            1
                :IP_TTL            2
                :IP_HDRINCL        3
                :IP_MTU           14
                :IP_FREEBIND      15
                :IP_NODEFRAG      22
                :IP_CHECKSUM      23}})
(def OPTIONS
  (merge OPTIONS*
         (into {} (map (fn [[o v]] [(get PROTOCOLS o) v]) OPTIONS*))))

(def bindings
  (.Library ffi nil
            (clj->js {"socket"     ["int" ["int" "int" "int"]]
                      "getsockopt" ["int" ["int" "int" "int" "pointer" "pointer"]]
                      "setsockopt" ["int" ["int" "int" "int" "string" "int"]]})))

(defn socket [domain typ protocol]
  (let [domain (if (number? domain) domain (get DOMAINS domain))
        typ (if (number? typ) type (get TYPES typ))
        protocol (if (number? protocol) protocol (get PROTOCOLS protocol))
        f-str  (str "socket(" domain ", " typ ", " protocol ")")
        fd (.socket bindings domain typ protocol)]
    (if (< fd 0)
      (throw (js/Error. (str "Could not " f-str ", Errno:" (.errno ffi))))
      (do
        (println "Called" f-str "sucessfully. Result:" fd)
        fd))))

(defn setsockopt [sock level option value]
  (let [fd (if (number? sock) sock ^number (.-_handle.fd sock))
        level-num (if (number? level) level (get PROTOCOLS level))
        option-num (if (number? option) option (get-in OPTIONS [level option]))
        buf (if (string? value) value (.alloc ref (.-types.int ref) value))
        sz (if (string? value) (.-length value) (.-types.int.size ref))
        f-str  (str "setsockopt(" fd ", " level ", " option ", " value ", " sz ")")
        res (.setsockopt bindings fd level-num option-num buf sz)]
    (if (< res 0)
      (throw (js/Error. (str "Could not " f-str ", Errno:" (.errno ffi))))
      (println "Called" f-str "sucessfully. Result:" res))))

;;;

(defn set-freebind [sock]
  (setsockopt sock :IPPROTO_IP :IP_FREEBIND 1))

(defn set-rcvbuf [sock buffsz]
  (setsockopt sock :SOL_SOCKET :SO_RCVBUF buffsz))

(defn bind-to-device [sock ifname]
  (setsockopt sock :SOL_SOCKET :SO_BINDTODEVICE ifname))

(defn set-reuse-port [sock]
  (setsockopt sock :SOL_SOCKET :SO_REUSEPORT 1))
