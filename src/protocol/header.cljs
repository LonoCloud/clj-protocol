(ns protocol.header)

(def DEFAULT-BUF-SIZE 1500)

(defn header->lookup
  "Take a 'spec' sequence and returns a map of key name to a map
  of field properties from 'spec' for that field."
  [spec]
  (loop [spec spec
         offset 0
         res {}]
    (let [[field-def & spec] spec
          [fname ftype flength fdefault] field-def
          fend (+ offset flength)
          res (assoc res fname {:name fname
                                :type ftype
                                :length flength
                                :default fdefault
                                :start offset
                                :end fend})]
      (if (seq spec)
        (recur spec fend res)
        res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compound reader and writer functions

(defn get-end
  [buf msg-map offset flen & [res]]
  (cond (number? flen)  (+ offset flen)
        (= :*    flen)  (if res res (.-length buf))
        :else           (+ offset (get msg-map flen))))

(defn read-header
  "Takes [buf start end ctx] and parses 'buf' based on '(:spec ctx)'
  using reader functions in '(:readers ctx)'. Returns a map of parsed
  fields with metdata '{:protocol/end end}' (where end is offset after
  read).

  Parameters:
  - 'buf': is the js/Buffer to read from
  - 'start': start reading from buf at this offset
  - 'end': stop reading at this offset (uses buffer length if nil)
  - 'ctx': a map of additional parsing context:
    - 'readers': map of types to reader functions
      - reader functions take [buf start end ctx]
    - 'spec': sequence of [name, type, lenth, default, extra-ctx]
      - 'name': field key used in returned map.
      - 'type': type to lookup in readers map
      - 'length': length of field and can be:
        - number: the number of bytes from the start of field (offset)
        - field: the name of a previously read field that contains
          number of bytes from start of the field (offset)
        - ':*': the rest of the buffer
      - 'default': ignored for reading
      - 'extra-ctx': extra context merged into ctx before reading
    - 'msg-map': map of values previously read
  "
  [buf start end {:keys [readers spec] :as ctx}]
  (loop [fields spec
         offset start
         msg-map {}]
    ;;(prn :read-header1 :offset offset :end end :-length (.-length buf) :field-cnt (count fields))
    (if (or (empty? fields) (>= offset (or end (.-length buf))))
      (with-meta msg-map {:protocol/end offset})
      (let [[[fname ftype flen fdefault fctx] & fields] fields
            fend (get-end buf msg-map offset flen end)
            reader (readers ftype)
            _ (assert reader (str "No reader for " ftype))
            ctx (merge ctx fctx {:msg-map msg-map})
            value (reader buf offset fend ctx)
            ;;_ (prn :read-header2 :fname fname :flen flen :fend fend :mend (-> value meta :protocol/end))
            fend (get-end buf msg-map offset flen (-> value meta :protocol/end))
            ;;_ (prn :read-header3 :fend fend)
            msg-map (assoc msg-map fname value)]
        (recur fields fend msg-map)))))

(defn write-header
  "Takes [buf msg-map start ctx] and encodes msg-map data
  into 'buf' based on '(:spec ctx)' using writer functions in
  '(:writers ctx)'. Returns offset after written data.

  Parameters:
  - 'buf': is the js/Buffer to write into (if nil then allocates
    js/Buffer of DEFAULT-BUF-SIZE bytes)
  - 'msg-map': map of field names -> field values to encode.
  - 'start': start writing at offset
  - 'ctx': a map of additional parsing context:
    - 'writers' is a map of types to writer functions
      - writer functions that take [buf val start ctx]
    - 'spec': sequence of [name, type, lenth, default]
      - 'name': field key to lookup in msg-map.
      - 'type': type to lookup in writers map
      - 'length': ignored for writing
      - 'default': if lookup is not set and msg-map does not contain
        'name' then 'default' is used instead
      - 'extra-ctx': extra context merged into ctx before writing
    - 'msg-map': parent msg-map for compound writers
  "
  [buf msg-map start {:keys [writers spec] :as ctx}]
  (loop [fields spec
         offset start]
    (if (or (empty? fields) (>= offset (.-length buf)))
      offset
      (let [[[fname ftype flen fdefault fctx] & fields] fields
            writer (writers ftype)
            _ (assert writer (str "No writer for " ftype))
            value (if (contains? msg-map fname)
                    (get msg-map fname)
                    fdefault)
            ctx (merge ctx fctx {:msg-map msg-map})
            ;; For fixed sized fields, ignore bytes written
            res (writer buf value offset ctx)
            fend (get-end buf msg-map offset flen res)]
        (recur fields fend)))))

;;;

(def ^{:doc "Alias for read-header."} read-header-full read-header)

(defn write-header-full
  "Like write-header but allocates a default sized buffer if not
  provided and returns the encoded js/Buffer sliced to size of the
  actual written data.
  "
  [buf msg-map start ctx]
  (let [buf (cond (not buf)     (.alloc js/Buffer DEFAULT-BUF-SIZE)
                  (number? buf) (.alloc js/Buffer buf)
                  :else         buf)
        end (write-header buf msg-map start ctx)]
    (.slice buf 0 end)))

(def readers
  {:header  read-header})

(def writers
  {:header  write-header})
