;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns protocol.tlvs
  "Read, write, and manipulate TLV (type-length-value) data.

  Reader functions take `[buf start ctx]` and return `[end value]`
  where `end` is the offset in `buf` after the read value(s).

  Writer functions take `[buf tlv-or-tlvs start ctx]` and return `end`
  where `end` is the offset in `buf` after the written value(s).

  https://en.wikipedia.org/wiki/Type%E2%80%93length%E2%80%93value"

  (:require [protocol.fields :as fields]))


(defn tlv-list->lookup
  "Takes a sequence of `[code, name, internal type, extra-context]`
  and converts it into a TLV lookup map containing:

  * `:list`  - original list/sequence
  * `:types` - map of codes and names to types
  * `:ctxs`  - map of codes and names to contexts
  * `:names` - map of names to codes
  * `:codes` - map of codes to names
  "
  [tlist]
  {:list tlist
   :types (fields/list->lookup tlist [0 2] [1 2])
   :ctxs  (fields/list->lookup tlist [0 3] [1 3])
   :names (fields/list->lookup tlist [1 0])
   :codes (fields/list->lookup tlist [0 1])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLV reader/writer (Type-Length-Value)

(defn read-tlv
  "Read a TLV (type-length-value) field from `buf` starting at
  `start`. The byte sizes of the type and length parts are specified by
  `tlv-tsize` and `tlv-lsize` respectively. The type value is used to
  lookup the reader and context from the `lookup` map (which should be
  constructed using [[tlv-list->lookup]]) which will be used to read
  the TLV value part.

  The returned value will be `[tlv-name tlv-value]` where `tlv-name`
  is the looked up type code and `tlv-value` is the value read from
  the field. If the looked up type is `:tlv-stop` then a true value
  will be appended to the returned tuple to indicate to looping
  readers that there are no more TLVs to read."
  [buf start {:keys [readers lookup tlv-tsize tlv-lsize] :as ctx}]
  (let [ctype (get {1 :uint8 2 :uint16} tlv-tsize)
        ltype (get {1 :uint8 2 :uint16} tlv-lsize)
        [_ code] ((readers ctype) buf start ctx)
        ttype (get-in lookup [:types code])
        _ (assert ttype (str "No TLV lookup definition for code " code))
        tname (get-in lookup [:codes code])
        tctx (get-in lookup [:ctxs code])
        lstart (+ start tlv-tsize)]
    (if (= :tlv-stop ttype)
      [lstart [tname nil] true]
      (let [[_ len] ((readers ltype) buf (+ tlv-tsize start) ctx)
            vstart (+ lstart tlv-lsize)
            ctx (merge ctx tctx {:length len})
            [vend value] ((readers ttype) buf vstart ctx)]
        [vend [tname value]]))))

(defn write-tlv
  "Write a TLV (type-length-value) field into `buf` starting at
  `start`. The byte sizes of the type and length parts are specified
  by `tlv-tsize` and `tlv-lsize` respectively. The `type-name` is used
  to lookup the type code and also the writer and context from the
  `lookup` map (which should be constructed using
  [[tlv-list->lookup]]) which will be used to write the TLV value
  part.

  If the type of the TLV is `:tlv-stop` then only the type part of the
  TLV will be written with the stop code (no length or value part will
  be written)."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLV collection readers/writers

(defn read-tlv-seq
  "Use [[fields/read-loop]] to read multiple TLVs from `buf` starting
  at `start` and ending when a stop value is read or length is
  reached."
  [buf start {:keys [length] :as ctx}]
  (fields/read-loop buf start (assoc ctx :loop-type :tlv)))

(defn read-tlv-map
  "Same as [[read-tlv-seq]] but returns a map of TLV names to values

  Note: repeats of the same TLV code/name will overwrite earlier
  values."
  [buf start ctx]
  (let [[fend tlvs] (read-tlv-seq buf start ctx)]
    [fend (into {} tlvs)]))

(defn write-tlv-seq
  "Use [[fields/write-loop]] to write multiple TLVs (`tlvs`) into
  `buf` starting at `start`."
  [buf tlvs start ctx]
  (fields/write-loop buf tlvs start (assoc ctx :loop-type :tlv)))

(defn write-tlv-map
  "Same as [[write-tlv-seq]] but takes a TLV map (`tlv-map`) rather
  than a a sequence of TLVs. Uses `(:list lookup)` to determine the
  order to write TLVs from `tlv-map`."
  [buf tlv-map tlv-start {:keys [lookup] :as ctx}]
  (let [tlvs (for [[c n t l] (:list lookup)
                   :when (contains? tlv-map n)]
               [n (get tlv-map n)])]
    (write-tlv-seq buf tlvs tlv-start ctx)))


;;; TLV manipulation

(defn get-in-tlv*
  "Takes a `tlvs` TLV hierachy/sequence and a `path` which is sequence
  of TLV type names to use in traversing the hierarchy. The return
  value is a sequence of matching TLV values and may be multiple if
  multiple TLVs of the same type are found at the leaf location."
  [tlvs path]
  (let [[p1 & path] path]
    (let [vs (for [[id v] tlvs :when (= p1 id)] v)]
      (if (empty? path)
        vs
        (seq (mapcat #(get-in-tlv* %1 path) vs))))))

(defn update-in-tlv*
  "Update TLV values in the `tlvs` at `path`. `tlvs` is a TLV
  hierarchy/sequence and `path` is a sequence of TLV type names to use
  in traversing the hierachy. Any matching TLV values will be updated
  using `(apply f tlv-value args)`."
  [tlvs path f & args]
  (let [[p1 & path] path]
    (vec
      (for [[id v] tlvs]
        (if (= p1 id)
          (if (empty? path)
            [id (apply f v args)]
            [id (apply update-in-tlv* v path f args)])
          [id v])))))

(defn assoc-in-tlv*
  "Replaces TLV values in the `tlvs` at `path`. `tlvs` is a TLV
  hierarchy/sequence and `path` is a sequence of TLV type names to use
  in traversing the hierachy. Any matching TLV values will be replaced
  with `value`."
  [tlvs path value]
  (update-in-tlv* tlvs path (fn [& xs] value)))

(defn update-in-tlv
  "Version of [[update-in-tlv*]] that takes a `msg-map` containing
  a `:tlvs` key where the TLV sequence will be updated."
  [msg-map & args]
  (apply update msg-map :tlvs update-in-tlv* args))

(defn assoc-in-tlv
  "Version of [[assoc-in-tlv*]] that takes a `msg-map` containing
  a `:tlvs` key where the TLV sequence will be updated."
  [msg-map path value]
  (update msg-map :tlvs assoc-in-tlv* path value))

(defn get-in-tlv
  "Version of [[get-in-tlv*]] that takes a `msg-map` containing
  a `:tlvs` key where the TLV sequence will searched."
  [msg-map path]
  (get-in-tlv* (:tlvs msg-map) path))

;;;

(def ^{:doc "TLV readers"}
  readers
  {:tlv     read-tlv
   :tlv-seq read-tlv-seq
   :tlv-map read-tlv-map})

(def ^{:doc "TLV writers"}
  writers
  {:tlv     write-tlv
   :tlv-seq write-tlv-seq
   :tlv-map write-tlv-map})
