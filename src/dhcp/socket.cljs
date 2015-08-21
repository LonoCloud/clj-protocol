(ns dhcp.socket)

(def ffi (js/require "ffi"))

(def SOL_SOCKET 1)
(def SO_BINDTODEVICE 25)

(def bindings
  (.Library ffi nil (clj->js {"setsockopt"
                              ["int" ["int" "int" "int" "string" "int"]]})))


(defn bind-to-device [sock ifname]
  (let [r (.setsockopt bindings
                       (-> sock .-_handle .-fd)
                       SOL_SOCKET
                       SO_BINDTODEVICE
                       ifname
                       (.-length ifname))]
    (when (< r 0)
      (println "Could not setsockopt SO_BINDTODEVICE on" ifname))))
