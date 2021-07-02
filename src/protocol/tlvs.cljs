(ns protocol.tlvs)

;; https://en.wikipedia.org/wiki/Type%E2%80%93length%E2%80%93value

;; list of [code, internal type, name]
(defn tlv-list->lookup [tlist]
  {:list tlist
   :types (merge (into {} (map (fn [[c n t l]] [c t]) tlist))
                 (into {} (map (fn [[c n t l]] [n t]) tlist)))
   :lookup (merge (into {} (map (fn [[c n t l]] [c l]) tlist))
                  (into {} (map (fn [[c n t l]] [n l]) tlist)))
   :names (into {} (map (fn [[c n t l]] [n c]) tlist))
   :codes (into {} (map (fn [[c n t l]] [c n]) tlist))})

;; TLV readers

(defn read-tlv
  [ctype clen ltype llen buf start readers tlv-lookup]
  (let [code ((readers ctype) buf start)
        ttype (get-in tlv-lookup [:types code])
        tlookup (or (get-in tlv-lookup [:lookup code]) tlv-lookup)]
    (when (not ttype)
      (println (str "WARNING: No TLV reader for " code " treating as :raw")))
    (if (= :stop ttype)
      [code ttype 0 0]
      (let [len ((readers ltype) buf (+ clen start))
            end (+ clen llen len start)
            reader (readers (or ttype :raw))
            value (reader buf (+ clen llen start) end readers tlookup)]
        [code ttype len value]))))

(def read-tlv-1-1 (partial read-tlv :uint8  1 :uint8 1))
(def read-tlv-2-2 (partial read-tlv :uint16 2 :uint16 2))

(defn read-tlv-seq
  [ctype clen ltype llen buf tlv-start tlv-end readers tlv-lookup]
  (let [{:keys [types codes] :as lookup} tlv-lookup]
    (loop [res []
           tstart tlv-start]
      (if (>= tstart tlv-end)
        res
        (let [[tcode ttype tlen value] (read-tlv ctype clen ltype llen
                                                 buf tstart readers tlv-lookup)
              tend (+ clen llen tlen tstart)
              tname (get codes tcode tcode)
              res (conj res [tname value])]
          (if (= :stop ttype)
            res
            (recur res tend)))))))

(def read-tlv-seq-1-1 (partial read-tlv-seq :uint8  1 :uint8  1))
(def read-tlv-seq-2-2 (partial read-tlv-seq :uint16 2 :uint16 2))

;;;

(defn read-tlv-map
  [ctype clen ltype llen buf tlv-start tlv-end readers tlv-lookup]
  (into {} (read-tlv-seq ctype clen ltype llen buf tlv-start tlv-end readers tlv-lookup)))

(def read-tlv-map-1-1 (partial read-tlv-map :uint8  1 :uint8  1))
(def read-tlv-map-2-2 (partial read-tlv-map :uint16 2 :uint16 2))

;; TLV writers

(defn write-tlv
  [ctype clen ltype llen buf [tname tvalue] start writers tlv-lookup]
  (let [code (get-in tlv-lookup [:names tname])
        _ (assert code (str "No TLV lookup definition for " tname))
        vtype (get-in tlv-lookup [:types tname])
        vlookup (or (get-in tlv-lookup [:lookup tname]) tlv-lookup)
        vstart (+ start clen llen)
        end ((writers ctype) buf code           start)]
    (if (= :stop vtype)
      end
      (let [vwriter (writers vtype)
            _ (assert vwriter (str "No writer for " vtype))
            end ((writers vtype) buf tvalue         vstart writers vlookup)
            _   ((writers ltype) buf (- end vstart) (+ clen start))]
        end))))

(def write-tlv-1-1 (partial write-tlv :uint8  1 :uint8  1))
(def write-tlv-2-2 (partial write-tlv :uint16 2 :uint16 2))

(defn write-tlv-seq
  [ctype clen ltype llen buf tlvs tlv-start writers tlv-lookup]
  (loop [tend tlv-start
         tlvs tlvs]
    (if (not (seq tlvs))
      tend
      (let [tlv (first tlvs)
            tend (write-tlv ctype clen ltype llen
                            buf tlv tend writers tlv-lookup)]
        (recur tend (next tlvs))))))

(def write-tlv-seq-1-1 (partial write-tlv-seq :uint8  1 :uint8  1))
(def write-tlv-seq-2-2 (partial write-tlv-seq :uint16 2 :uint16 2))

;;;

(defn write-tlv-map
  [ctype clen ltype llen buf tlvs tlv-start writers tlv-lookup]
  (let [tlv-map (into {} tlvs)
        tlvs (for [[c n t l] (:list tlv-lookup)
                   :when (contains? tlv-map n)]
               [n (get tlv-map n)])]
    (write-tlv-seq ctype clen ltype llen buf tlvs tlv-start writers tlv-lookup)))

(def write-tlv-map-1-1 (partial write-tlv-map :uint8  1 :uint8  1))
(def write-tlv-map-2-2 (partial write-tlv-map :uint16 2 :uint16 2))

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
  {:tlv-seq-1-1 read-tlv-seq-1-1
   :tlv-seq-2-2 read-tlv-seq-2-2
   :tlv-map-1-1 read-tlv-map-1-1
   :tlv-map-2-2 read-tlv-map-2-2 })

(def writers
  {:tlv-seq-1-1 write-tlv-seq-1-1
   :tlv-seq-2-2 write-tlv-seq-2-2
   :tlv-map-1-1 write-tlv-map-1-1
   :tlv-map-2-2 write-tlv-map-2-2})
