(defproject dhcp "0.1.0-SNAPSHOT"
  :description "ClojureScript DHCP Server"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.107"]]

  :node-dependencies [[source-map-support "0.3.1"]]

  :plugins [[lein-cljsbuild "1.0.4"]
            [lein-npm "0.4.0"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "dhcp"
              :source-paths ["src"]
              :compiler {
                :output-to "out/dhcp.js"
                :output-dir "out"
                :target :nodejs
                :optimizations :none
                :source-map true}}]})
