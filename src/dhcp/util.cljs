(ns dhcp.util
  (:require [clojure.string :as string]
            [protocol.addrs :as addrs]))

(def os (js/require "os"))
(def fs (js/require "fs"))
(def execSync (.-execSync (js/require "child_process")))

(defn get-if-ipv4 [if-name]
  (let [srv-ifs (-> os .networkInterfaces (js->clj :keywordize-keys true))
        _ (prn :srv-ifs srv-ifs)
        srv-if-ipv4 (-> srv-ifs (get (keyword if-name)) first)
        {:keys [address netmask]} srv-if-ipv4]
    (assoc srv-if-ipv4
           :broadcast (addrs/broadcast address netmask))))

(defn set-ip-address [if-name address netmask]
  (let [prefix (addrs/mask-ip->prefix netmask)
        ip-cmd (str "addr flush dev " if-name "\n"
                    "addr add " address "/" prefix " dev " if-name "\n")]
    (println (str "Setting " if-name " to " address "/" prefix))
    (execSync "ip -o -b -" #js {:encoding "utf-8"
                                :input ip-cmd})))

(defn get-mac-address [if-name]
  (let [haddr-file (str "/sys/class/net/" if-name "/address")
        hw-addr (string/trim (.readFileSync fs haddr-file "utf8"))]
    hw-addr))

