(ns protocol.tlvs)

;; https://en.wikipedia.org/wiki/Type%E2%80%93length%E2%80%93value

;; list of [code, name, internal type, lookup, args]
(defn tlv-list->lookup [tlist]
  {:list tlist
   :types  (merge (into {} (map (fn [[c n t a]] [c t]) tlist))
                  (into {} (map (fn [[c n t a]] [n t]) tlist)))
   :args   (merge (into {} (map (fn [[c n t a]] [c a]) tlist))
                  (into {} (map (fn [[c n t a]] [n a]) tlist)))
   :names (into {} (map (fn [[c n t a]] [n c]) tlist))
   :codes (into {} (map (fn [[c n t a]] [c n]) tlist))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLV readers

(defn read-tlv
  "If the args for the current field type value start with ':self'
  then the current value of tlv-lookup will be substituted. This
  TLV fields to contain TLV fields that use the same definition/lookup."
  [buf start readers tlv-lookup clen llen]
  (let [ctype (get {1 :uint8 2 :uint16} clen)
        ltype (get {1 :uint8 2 :uint16} llen)
        code ((readers ctype) buf start)
        ttype (get-in tlv-lookup [:types code])
        targs (get-in tlv-lookup [:args code])
        targs (if (or (not targs) (= :self (first targs)))
                (cons tlv-lookup (drop 1 targs))
                targs)]
    (when (not ttype)
      (println (str "WARNING: No TLV reader for " code " treating as :raw")))
    (if (= :stop ttype)
      [code ttype 0 0]
      (let [len ((readers ltype) buf (+ clen start))
            vstart (+ clen llen start)
            vend   (+ clen llen start len)
            reader (readers (or ttype :raw))
            value (apply reader buf vstart  vend readers targs)]
        [code ttype len value]))))

(defn read-tlv-seq
  [buf tlv-start tlv-end readers tlv-lookup clen llen]
  (let [{:keys [types codes] :as lookup} tlv-lookup]
    (loop [res []
           tstart tlv-start]
      (if (>= tstart tlv-end)
        res
        (let [[tcode ttype tlen value] (read-tlv buf tstart readers
                                                 tlv-lookup clen llen)
              tend (+ clen llen tlen tstart)
              tname (get codes tcode tcode)
              res (conj res [tname value])]
          (if (= :stop ttype)
            res
            (recur res tend)))))))

;;;

(defn read-tlv-map
  [buf tlv-start tlv-end readers tlv-lookup clen llen]
  (into {} (read-tlv-seq buf tlv-start tlv-end readers tlv-lookup clen llen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLV writers

(defn write-tlv
  "If the args for the current field type value start with ':self'
  then the current value of tlv-lookup will be substituted. This
  TLV fields to contain TLV fields that use the same definition/lookup."
  [buf [tname tvalue] start writers tlv-lookup clen llen]
  (let [code (get-in tlv-lookup [:names tname])
        _ (assert code (str "No TLV lookup definition for " tname))
        ctype (get {1 :uint8 2 :uint16} clen)
        ltype (get {1 :uint8 2 :uint16} llen)
        vtype (get-in tlv-lookup [:types tname])
        vargs (get-in tlv-lookup [:args tname])
        vargs (if (or (not vargs) (= :self (first vargs)))
                (cons tlv-lookup (drop 1 vargs))
                vargs)
        vstart (+ start clen llen)
        end ((writers ctype) buf code start)]
    (if (= :stop vtype)
      end
      (let [vwriter (writers vtype)
            _ (assert vwriter (str "No writer for " vtype))
            end (apply vwriter   buf tvalue         vstart writers vargs)
            _   ((writers ltype) buf (- end vstart) (+ clen start))]
        end))))

(defn write-tlv-seq
  [buf tlvs tlv-start writers tlv-lookup clen llen]
  (loop [tend tlv-start
         tlvs tlvs]
    (if (not (seq tlvs))
      tend
      (let [tlv (first tlvs)
            tend (write-tlv buf tlv tend writers tlv-lookup clen llen)]
        (recur tend (next tlvs))))))

;;;

(defn write-tlv-map
  [buf tlvs tlv-start writers tlv-lookup clen llen]
  (let [tlv-map (into {} tlvs)
        tlvs (for [[c n t l] (:list tlv-lookup)
                   :when (contains? tlv-map n)]
               [n (get tlv-map n)])]
    (write-tlv-seq buf tlvs tlv-start writers tlv-lookup clen llen)))


;;; TLV manipulation

(defn get-in-tlv* [tlvs path]
  (let [[p1 & path] path]
    (let [vs (for [[id v] tlvs :when (= p1 id)] v)]
      (if (empty? path)
        vs
        (seq (mapcat #(get-in-tlv* %1 path) vs))))))

(defn update-in-tlv* [tlvs path f & args]
  (let [[p1 & path] path]
    (vec
      (for [[id v] tlvs]
        (if (= p1 id)
          (if (empty? path)
            [id (apply f v args)]
            [id (apply update-in-tlv* v path f args)])
          [id v])))))

(defn assoc-in-tlv* [tlvs path value]
  (update-in-tlv* tlvs path (fn [& xs] value)))

(defn update-in-tlv [msg-map & args]
  (apply update msg-map :tlvs update-in-tlv* args))
(defn assoc-in-tlv [msg-map path value]
  (update msg-map :tlvs assoc-in-tlv* path value))
(defn get-in-tlv [msg-map path]
  (get-in-tlv* (:tlvs msg-map) path))

;;;

(set! *warn-on-infer* false)

(def readers
  {:tlv-seq read-tlv-seq
   :tlv-map read-tlv-map })

(def writers
  {:tlv-seq write-tlv-seq
   :tlv-map write-tlv-map})
