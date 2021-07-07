(ns dhcp.util
  "Interface and file I/O utility functions."
  (:require [clojure.string :as string]
            [protocol.addrs :as addrs]))

(def ^:private os (js/require "os"))
(def ^:private fs (js/require "fs"))
(def ^:private execSync (.-execSync (js/require "child_process")))

(defn existsSync "Return true if a file exists at `path`"
  [path] (.existsSync fs path))
(defn slurp "Synchronously read file at `path`"
  [path] (.readFileSync fs path "utf-8"))
(defn spit "Synchronously write `data` to `path`"
  [path data] (.writeFileSync fs path data))


(defn get-if-ipv4
  "Get interface information for the interface `if-name`"
  [if-name]
  (let [srv-ifs (-> os .networkInterfaces (js->clj :keywordize-keys true))
        _ (prn :srv-ifs srv-ifs)
        srv-if-ipv4 (-> srv-ifs (get (keyword if-name)) first)
        {:keys [address netmask]} srv-if-ipv4]
    (assoc srv-if-ipv4
           :broadcast (addrs/broadcast address netmask))))

(defn set-ip-address
  "Set the `address` and `netmask` for the interface `if-name`"
  [if-name address netmask]
  (let [prefix (addrs/mask-ip->prefix netmask)
        ip-cmd (str "addr flush dev " if-name "\n"
                    "addr add " address "/" prefix " dev " if-name "\n")]
    (println (str "Setting " if-name " to " address "/" prefix))
    (execSync "ip -o -b -" #js {:encoding "utf-8"
                                :input ip-cmd})))

(defn get-mac-address
  "Return the MAC address for the interface `if-name`"
  [if-name]
  (let [haddr-file (str "/sys/class/net/" if-name "/address")
        hw-addr (string/trim (.readFileSync fs haddr-file "utf8"))]
    hw-addr))

