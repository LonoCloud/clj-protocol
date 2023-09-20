(ns dhcp.logging)

;;
;; Efficient logging
;;
(def log-config (atom {}))

(defn log-message
  "Print a log message for the message in `msg`. If kind is :info or
  :error then `msg` is a string log message. Otherwise if kind is
  :send or :recv then treat `msg` as a msp-map structure and print
  a sent or received message."
  [kind msg remote]
  (let [{:keys [log-level log-prefix]} @log-config
        msg-type (or (#{:info :error} kind) (:opt/msg-type msg))]
    (condp = log-level
      0 nil
      1 (do (js/process.stdout.write (condp = msg-type
                                       :error "!"
                                       :info "."
                                       :start "+"
                                       :DISCOVER ")"
                                       :OFFER "("
                                       :REQUEST ">"
                                       :ACK "<"
                                       :NAK "X"
                                       :RELEASE "R"
                                       "?"))
            (swap! log-config update :pending inc))
      2 (if (#{:send :recv} kind)
          (println (str log-prefix msg-type
                        (if (#{:send} kind) " to " " from ") remote
                        " for " (:chaddr msg)
                        (if (:yiaddr msg) (str ", yiaddr " (:yiaddr msg)))))
          (println (str log-prefix (name msg-type) ": " msg))))
    nil))

;; Flush log output periodically (for log-level 1)
(defn log-flush []
  (swap! log-config #(if (> (:pending %) 0)
                       (do
                         (when (= 1 (:log-level %))
                           (js/process.stdout.write "\n"))
                         (assoc % :pending 0))
                       %)))

(defn start-logging [{:keys [log-prefix] :as cfg}]
  (let [log-prefix (if (empty? log-prefix) "" (str log-prefix " "))]
    (reset! log-config (merge cfg {:pending 0 :log-prefix log-prefix})))
  (js/setInterval log-flush 500))
