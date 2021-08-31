;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns protocol.util
  "Protocol platform specific print and debug utilities"
  (:require [clojure.string :as string]
            [protocol.platform :as plat]))

(defn- chunk-buf [buf {:keys [columns start end base]
                      :or {columns 20 start 0 base 16}}]
  (let [octets (map #(plat/pr-num % base 2)
                    (plat/buf->vec buf start (or end (plat/buf-len buf))))]
    (map #(string/join " " %)
         (partition columns columns (repeat "  ") octets))))

(defn pr-buf
  "Pretty print the contents of a js/Buffer"
  [buf & [{:keys [prefix] :as opts :or {prefix "  "}}]]
  (let [indent (apply str (repeat (count prefix) " "))
        chunks (chunk-buf buf opts)]
    (str prefix
         (string/join (str "\n" indent) chunks))))

(defn pr-bufs
  "Pretty print the contents of multiple js/Buffer's side-by-side"
  [bufs & [{:keys [prefix] :as opts
                        :or {prefix "  "}}]]
  (let [indent (apply str (repeat (count prefix) " "))
        bufs-chunks (map #(chunk-buf % opts) bufs)
        buf-strs (apply map #(string/join " | " %&) bufs-chunks)]
    (str prefix
         (string/join (str "\n" indent) buf-strs))))
