(ns protocol.socket)

(def ffi (js/require "@saleae/ffi"))
(def ref (js/require "ref"))

(def SOL_SOCKET 0x1)

(def SO_RCVBUF       0x08)
(def SO_REUSEPORT    0x0f)
(def SO_BINDTODEVICE 0x19)

(def bindings
  (.Library ffi nil
            (clj->js {"getsockopt" ["int" ["int" "int" "int" "pointer" "pointer"]]
                      "setsockopt" ["int" ["int" "int" "int" "string" "int"]]})))

(defn set-rcvbuf [sock buffsz]
  (let [fd ^number (.-_handle.fd sock)
        buff (.alloc ref (.-types.int ref) buffsz)
        r (.setsockopt bindings fd SOL_SOCKET SO_RCVBUF buff (.-types.int.size ref))]
    (when (< r 0)
      (throw (js/Error. (str "Could not setsockopt SO_RCVBUF on" fd ", Errno:" (.errno ffi)))))))

(defn bind-to-device [sock ifname]
  (let [fd ^number (.-_handle.fd sock)
        r (.setsockopt bindings fd SOL_SOCKET SO_BINDTODEVICE ifname (.-length ifname))]
    (when (< r 0)
      (throw (js/Error. (str "Could not setsockopt SO_BINDTODEVICE on" ifname ", Errno:" (.errno ffi)))))))

(defn reuse-port [sock]
  (let [fd ^number (.-_handle.fd sock)
        buff (.alloc ref (.-types.int ref) 1)
        r (.setsockopt bindings fd SOL_SOCKET SO_REUSEPORT buff (.-types.int.size ref))]
    (when (< r 0)
      (throw (js/Error. (str "Could not setsockopt SO_REUSEPORT, Errno:" (.errno ffi)))))))
