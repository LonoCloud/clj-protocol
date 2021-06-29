(ns protocol.header)

(def DEFAULT-BUF-SIZE 1500)

(defn header->lookup
  "Take a 'header-def' sequence and returns a map of key name to a map
  of field properties from 'header-def' for that field."
  [header-def]
  (loop [header-def header-def
         offset 0
         res {}]
    (let [[field-def & header-def] header-def
          [fname ftype flength fdefault flookup args] field-def
          fend (+ offset flength)
          res (assoc res fname {:name fname
                                :type ftype
                                :length flength
                                :default fdefault
                                :args args
                                :start offset
                                :end fend})]
      (if (seq header-def)
        (recur header-def fend res)
        res))))

(defn get-end
  [buf msg-map offset flen & [res]]
  (cond (number? flen)  (+ offset flen)
        (= :*    flen)  (if res res (.-length buf))
        (fn?     flen)  (+ offset (flen buf msg-map offset))
        :else           (+ offset (get msg-map flen))))

(defn read-header
  "Takes [buf start end readers header-def] and parses 'buf' based on
  'header-def' using reader functions in 'readers'. Returns a map of
  parsed fields.

  Parameters:
  - 'buf': is the js/Buffer to read from
  - 'start': start reading from buf at this offset
  - 'end': stop reading at this offset (uses buffer length if nil)
  - 'readers' is a map of types to reader functions
    - reader functions take [buf start end readers & args]
  - 'header-def' is a sequence of [name, type, lenth, default, args]
    - 'name': field key used in returned map.
    - 'type': type to lookup in readers map
    - 'length': length of field and can be:
      - number: the number of bytes from the start of field (offset)
      - field: the name of a previously read field that contains
        number of bytes from start of the field (offset)
      - ':*': the rest of the buffer
      - function: called with [buf msg-map offset] and returns the
        length of the field
    - 'default': ignored for reading
    - 'args': seq of additional argument to append to reader call.
  "
  [buf start end readers header-def]
  (loop [fields header-def
         offset start
         msg-map {}]
    (if (or (empty? fields) (>= offset (or end (.-length buf))))
      msg-map
      (let [[[fname ftype flen fdefault args] & fields] fields
            fend (get-end buf msg-map offset flen)
            reader (readers ftype)
            _ (assert reader (str "No reader for " ftype))
            value (apply reader buf offset fend readers args)
            msg-map (assoc msg-map fname value)]
        (recur fields fend msg-map)))))

(defn write-header
  "Takes [buf msg-map start writers header-def] and encodes msg-map data
  into 'buf' (allocated if not provided) based on 'header-def'
  using writer functions in 'writers'. Returns the encoded js/Buffer
  sliced to size of the actual written data.

  Parameters:
  - 'buf': is the js/Buffer to write into (if nil then allocates
    js/Buffer of DEFAULT-BUF-SIZE bytes)
  - 'msg-map': map of field names -> field values to encode.
  - 'start': start writing at offset
  - 'writers' is a map of types to writer functions
    - writer functions that take [buf val start writers & args]
  - 'header-def' is a sequence of [name type lenth default args]
    - 'name': field key to lookup in msg-map.
    - 'type': type to lookup in writers map
    - 'length': ignored for writing
    - 'default': if lookup is not set and msg-map does not contain
      'name' then 'default' is used instead and can be:
        - function: called with [buf msg-map offset writers] and the 
          result is used as the default value.
        - other: used as the default value directly.
    - 'args': seq of additional argument to append to writer call.
  "
  [buf msg-map start writers header-def]
  (let [buf (cond (not buf)     (.alloc js/Buffer DEFAULT-BUF-SIZE)
                  (number? buf) (.alloc js/Buffer buf)
                  :else         buf)]
    (loop [fields header-def
           offset start]
      (if (or (empty? fields) (>= offset (.-length buf)))
        (.slice buf 0 offset)
        (let [[[fname ftype flen fdefault args] & fields] fields
              writer (writers ftype)
              _ (assert writer (str "No writer for " ftype))
              value (if (contains? msg-map fname)
                      (get msg-map fname)
                      (if (fn? fdefault)
                        (fdefault buf msg-map offset writers)
                        fdefault))
              ;; For fixed sized fields, ignore bytes written
              res (apply writer buf value offset writers args)
              fend (get-end buf msg-map offset flen res)]
          (recur fields fend))))))

