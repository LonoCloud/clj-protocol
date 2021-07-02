(ns protocol.tlvs
  (:require [protocol.fields :as fields]))

;; https://en.wikipedia.org/wiki/Type%E2%80%93length%E2%80%93value

;; list of [code, name, internal type, extra-context]
(defn tlv-list->lookup [tlist]
  {:list tlist
   :types  (merge (into {} (map (fn [[c n t x]] [c t]) tlist))
                  (into {} (map (fn [[c n t x]] [n t]) tlist)))
   :ctxs    (merge (into {} (map (fn [[c n t x]] [c x]) tlist))
                  (into {} (map (fn [[c n t x]] [n x]) tlist)))
   :names (into {} (map (fn [[c n t x]] [n c]) tlist))
   :codes (into {} (map (fn [[c n t x]] [c n]) tlist))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLV readers

(defn read-tlv
  "Takes [buf start end ctx]. Returns [name value] with metadata
  '{:protocol/end end}' (where 'end' is offset after read)."
  [buf start end {:keys [readers lookup tlv-tsize tlv-lsize] :as ctx}]
  (let [ctype (get {1 :uint8 2 :uint16} tlv-tsize)
        ltype (get {1 :uint8 2 :uint16} tlv-lsize)
        code ((readers ctype) buf start nil ctx)
        ttype (get-in lookup [:types code])
        _ (assert ttype (str "No TLV lookup definition for code " code))
        tname (get-in lookup [:codes code])
        tctx (get-in lookup [:ctxs code])]
    (if (= :tlv-stop ttype)
      (vary-meta [tname nil] merge {:protocol/end (+ start tlv-tsize)
                                    :protocol/stop true})
      (let [len ((readers ltype) buf (+ tlv-tsize start) nil ctx)
            vstart (+ tlv-tsize tlv-lsize start)
            vend   (+ tlv-tsize tlv-lsize start len)
            value ((readers ttype) buf vstart  vend (merge ctx tctx))]
        (vary-meta [tname value] merge {:protocol/end vend})))))

(defn read-tlv-seq
  [buf start end ctx]
  (fields/read-loop buf start end (assoc ctx :loop-type :tlv)))

;;;

(defn read-tlv-map
  [buf start end ctx]
  (let [tlvs (read-tlv-seq buf start end ctx)]
    (with-meta (into {} tlvs) (meta tlvs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLV writers

(defn write-tlv
  "Takes [buf [name value] start ctx]. Returns offset after write."
  [buf [tlv-name tlv-value] start {:keys [writers lookup
                                          tlv-tsize tlv-lsize] :as ctx}]
  (let [ctype (get {1 :uint8 2 :uint16} tlv-tsize)
        ltype (get {1 :uint8 2 :uint16} tlv-lsize)
        code (get-in lookup [:names tlv-name])
        _ (assert code (str "No TLV lookup definition for " tlv-name))
        vtype (get-in lookup [:types tlv-name])
        vctx (get-in lookup [:ctxs tlv-name])
        vstart (+ start tlv-tsize tlv-lsize)
        end ((writers ctype) buf code start ctx)]
    (if (= :tlv-stop vtype)
      end
      (let [vwriter (writers vtype)
            _ (assert vwriter (str "No writer for " vtype))
            end (vwriter         buf tlv-value      vstart              (merge ctx vctx))
            _   ((writers ltype) buf (- end vstart) (+ tlv-tsize start) ctx)]
        end))))

(defn write-tlv-seq
  [buf value start ctx]
  (fields/write-loop buf value start (assoc ctx :loop-type :tlv)))

;;;

(defn write-tlv-map
  "Takes [buf tlv-map tlv-start ctx]. Returns offset after write."
  [buf tlv-map tlv-start {:keys [lookup] :as ctx}]
  (let [tlvs (for [[c n t l] (:list lookup)
                   :when (contains? tlv-map n)]
               [n (get tlv-map n)])]
    (write-tlv-seq buf tlvs tlv-start ctx)))


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

(def readers
  {:tlv     read-tlv
   :tlv-seq read-tlv-seq
   :tlv-map read-tlv-map})

(def writers
  {:tlv     write-tlv
   :tlv-seq write-tlv-seq
   :tlv-map write-tlv-map})
