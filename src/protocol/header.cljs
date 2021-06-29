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

(defn get-end
  [buf msg-map offset flen & [res]]
  (cond (number? flen)  (+ offset flen)
        (= :*    flen)  (if res res (.-length buf))
        :else           (+ offset (get msg-map flen))))

(defn read-header
  "Takes [buf start end ctx] and parses 'buf' based on '(:spec ctx)'
  using reader functions in '(:readers ctx)'. Returns a map of parsed
  fields.

  Parameters:
  - 'buf': is the js/Buffer to read from
  - 'start': start reading from buf at this offset
  - 'end': stop reading at this offset (uses buffer length if nil)
  - 'ctx': a map of additional parsing context:
    - 'readers': map of types to reader functions
      - reader functions take [buf start end ctx]
    - 'spec': sequence of [name, type, lenth, default]
      - 'name': field key used in returned map.
      - 'type': type to lookup in readers map
      - 'length': length of field and can be:
        - number: the number of bytes from the start of field (offset)
        - field: the name of a previously read field that contains
          number of bytes from start of the field (offset)
        - ':*': the rest of the buffer
      - 'default': ignored for reading
  "
  [buf start end {:keys [readers spec] :as ctx}]
  (loop [fields spec
         offset start
         msg-map {}]
    (if (or (empty? fields) (>= offset (or end (.-length buf))))
      msg-map
      (let [[[fname ftype flen fdefault fctx] & fields] fields
            fend (get-end buf msg-map offset flen)
            reader (readers ftype)
            _ (assert reader (str "No reader for " ftype))
            value (reader buf offset fend (merge ctx fctx))
            msg-map (assoc msg-map fname value)]
        (recur fields fend msg-map)))))

(defn write-header
  "Takes [buf msg-map start ctx] and encodes msg-map data
  into 'buf' (allocated if not provided) based on '(:spec ctx)'
  using writer functions in '(:writers ctx)'. Returns the encoded
  js/Buffer sliced to size of the actual written data.

  Parameters:
  - 'buf': is the js/Buffer to write into (if nil then allocates
    js/Buffer of DEFAULT-BUF-SIZE bytes)
  - 'msg-map': map of field names -> field values to encode.
  - 'start': start writing at offset
  - 'ctx': a map of additional parsing context:
    - 'writers' is a map of types to writer functions
      - writer functions that take [buf val start ctx]
      - 'name': field key to lookup in msg-map.
      - 'type': type to lookup in writers map
      - 'length': ignored for writing
      - 'default': if lookup is not set and msg-map does not contain
        'name' then 'default' is used instead
  "
  [buf msg-map start {:keys [writers spec] :as ctx}]
  (let [buf (cond (not buf)     (.alloc js/Buffer DEFAULT-BUF-SIZE)
                  (number? buf) (.alloc js/Buffer buf)
                  :else         buf)]
    (loop [fields spec
           offset start]
      (if (or (empty? fields) (>= offset (.-length buf)))
        (.slice buf 0 offset)
        (let [[[fname ftype flen fdefault fctx] & fields] fields
              writer (writers ftype)
              _ (assert writer (str "No writer for " ftype))
              value (if (contains? msg-map fname)
                      (get msg-map fname)
                      fdefault)
              ;; For fixed sized fields, ignore bytes written
              res (writer buf value offset (merge ctx fctx))
              fend (get-end buf msg-map offset flen res)]
          (recur fields fend))))))

