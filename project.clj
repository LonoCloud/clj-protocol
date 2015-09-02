(defproject dhcp "0.1.0-SNAPSHOT"
  :description "ClojureScript DHCP Server"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.107"]]

  :node-dependencies [[source-map-support "0.3.1"]
                      [ffi "1.3.2"]]

  :plugins [[lein-cljsbuild "1.0.4"]
            [lein-npm "0.4.0"]]

  :source-paths ["src"]

  :cljsbuild {:builds {:dev {:source-paths ["src"]
                             :compiler {:output-to "out/dhcp.js"
                                        :output-dir "out"
                                        :target :nodejs
                                        :optimizations :none
                                        :source-map true}}
                       :lib {:source-paths ["src/dhcp/"]
                             :compiler {:output-to "lib/dhcp.js"
                                        :target :nodejs
                                        :optimizations :simple}}
                       :release {:source-paths ["src/"]
                                 :compiler {:output-to "dhcp.js"
                                            :target :nodejs
                                            :optimizations :simple}}}})
