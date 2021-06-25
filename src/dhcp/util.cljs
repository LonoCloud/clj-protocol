(ns dhcp.util
  (:require [protocol.fields :as fields]))

(def os (js/require "os"))
(def execSync (.-execSync (js/require "child_process")))

(defn get-if-ipv4 [if-name]
  (let [srv-ifs (-> os .networkInterfaces (js->clj :keywordize-keys true))
    _ (prn srv-ifs)
        srv-if-ipv4 (-> srv-ifs (get (keyword if-name)) first)
        {:keys [address netmask]} srv-if-ipv4]
    (assoc srv-if-ipv4
      :octets {:address (fields/ip->octet address)
               :netmask (fields/ip->octet netmask)
               :broadcast (fields/ip->octet (fields/broadcast address netmask))
               :mac (fields/mac->octet (:mac srv-if-ipv4))})))

(defn set-address [if-name address netmask]
  (let [prefix (fields/mask-ip->prefix netmask)
        ip-cmd (str "addr flush dev " if-name "\n"
                    "addr add " address "/" prefix " dev " if-name "\n")]
    (println (str "Setting " if-name " to " address "/" prefix))
    (execSync "ip -o -b -" #js {:encoding "utf-8"
                                :input ip-cmd})))



