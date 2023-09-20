;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns dhcp.util
  "Interface and file I/O utility functions."
  (:require [clojure.string :as string]
            [protocol.addrs :as addrs]
            ["os" :as os]
            ["fs" :as fs]
            ["child_process" :refer [execSync]]))

(def Eprn #(binding [*print-fn* *print-err-fn*] (apply prn %&)))
(def Eprintln #(binding [*print-fn* *print-err-fn*] (apply println %&)))
(defn fatal [code & msg] (apply Eprintln msg) (js/process.exit code))

(defn existsSync "Return true if a file exists at `path`"
  [path] (fs/existsSync path))
(defn slurp "Synchronously read file at `path`"
  [path] (fs/readFileSync path "utf-8"))
(defn spit "Synchronously write `data` to `path`"
  [path data] (fs/writeFileSync path data))

(defn deep-merge' [a b] (merge-with #(if (map? %2) (deep-merge' %1 %2) %2) a b))
(defn deep-merge [x & xs] (reduce deep-merge' x xs))

(defn get-ifs-ipv4
  "Get IPv4 information for all interfaces"
  []
  (into {} (for [[k intfs] (-> (os/networkInterfaces) ->clj)
                 :let [intf (first (filter #(= "IPv4" (:family %)) intfs))
                       {:keys [address netmask]} intf
                       broadcast (addrs/broadcast address netmask)]]
             [(name k) (assoc intf :broadcast broadcast)])))

(defn get-if-ipv4
  "Get interface information for the interface `if-name`"
  [if-name]
  (get (get-ifs-ipv4) if-name))

(defn set-ip-address
  "Set the `address` and `netmask` for the interface `if-name`"
  [if-name address netmask]
  (let [prefix (addrs/mask-ip->prefix netmask)
        ip-cmd (str "addr flush dev " if-name "\n"
                    "addr add " address "/" prefix " dev " if-name "\n")]
    (execSync "ip -o -b -" #js {:encoding "utf-8"
                                :input ip-cmd})))

(defn get-mac-address
  "Return the MAC address for the interface `if-name`"
  [if-name]
  (let [haddr-file (str "/sys/class/net/" if-name "/address")
        hw-addr (string/trim (fs/readFileSync haddr-file "utf8"))]
    hw-addr))

