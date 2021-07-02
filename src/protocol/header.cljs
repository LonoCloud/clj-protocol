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
          [fname ftype flength fdefault flookup] field-def
          fend (+ offset flength)
          res (assoc res fname {:name fname
                                :type ftype
                                :length flength
                                :default fdefault
                                :lookup flookup
                                :start offset
                                :end fend})]
      (if (seq header-def)
        (recur header-def fend res)
        res))))

(defn read-header
  "Takes [buf start end readers header-def] and parses 'buf' based on
  'header-def' using reader functions in 'readers'. Returns a map of
  parsed fields.

  Parameters:
  - 'buf': is the js/Buffer to read from
  - 'start': start reading from buf at this offset
  - 'end': stop reading at this offset (uses buffer length if nil)
  - 'readers' is a map of types to reader functions
    - reader functions take [buf start end & [readers lookup]]
  - 'header-def' is a sequence of [name, type, lenth, default, lookup]
    - 'name': field key used in returned map. If 'name' is ':INTO'
      then the value from reading the key is assumed to be a map
      (compound) and merged into the result map.
    - 'type': type to lookup in readers map
    - 'length': length of field and can be:
      - number: the number of bytes from the start of field (offset)
      - ':*': the rest of the buffer
      - field: the name of a previously read field that contains
        number of bytes from start of the field (offset)
      - ':->1-1': length of 'value' is read 1 byte past offset and
        is 1 byte long
      - ':->2-2': length of 'value' is read 2 bytes past offset and
        is 2 bytes long
    - 'default': ignored for reading
    - 'lookup': if 'type' is compound (e.g. TLV) then 'lookup'
      provides additional data for reading internal data.
  "
  [buf start end readers header-def]
  (loop [fields header-def
         offset start
         msg-map {}]
    (if (or (empty? fields) (>= offset (or end (.-length buf))))
      msg-map
      (let [[[fname ftype flen fdefault flookup] & fields] fields
            fend (cond (number? flen)  (+ offset flen)
                       (= :->1-1 flen) (+ 2 offset ((readers :uint8) buf (+ 1 offset)))
                       (= :->2-2 flen) (+ 4 offset ((readers :uint16) buf (+ 2 offset)))
                       (= :*     flen) (.-length buf)
                       :else           (+ offset (get msg-map flen)))
            reader (readers ftype)
            _ (assert reader (str "No reader for " ftype))
            value (reader buf offset fend readers flookup)
            msg-map (if (= :INTO fname)
                      (into msg-map value)
                      (assoc msg-map fname value))]
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
    - writer functions take [buf val start & [writers lookup]]
  - 'header-def' is a sequence of [name, type, lenth, default, lookup]
    - 'name': field key to lookup in msg-map. If 'name is ':INTO' 
      values for the field are instead extracted from the 'msg-map'
      based on the definition in the 'lookup' map.
    - 'type': type to lookup in writers map
    - 'length': length of field and can be:
      - number: the number of bytes from the start of field (offset)
      - ':*': the rest of the buffer
      - field: the name of a field from msg-map that contains
        number of bytes from start of the field (offset)
      - ':->1-1': length of 'value' is written 1 byte past offset and
        is 1 byte long
      - ':->2-2': length of 'value' is written 2 bytes past offset and
        is 2 bytes long
    - 'default': if msg-map does not contain 'name' then this value
      will be used instead
    - 'lookup': if a lookup is defined then this is a compound field
      (e.g.  TLV) and the writer is called with 'writers' and 'lookup'
      as extra parameters.
  "
  [buf msg-map start writers header-def]
  (let [buf (cond (not buf)     (.alloc js/Buffer DEFAULT-BUF-SIZE)
                  (number? buf) (.alloc js/Buffer buf)
                  :else         buf)]
    (loop [fields header-def
           offset start]
      (if (or (empty? fields) (>= offset (.-length buf)))
        (.slice buf 0 offset)
        (let [[[fname ftype flen fdefault flookup] & fields] fields
              writer (writers ftype)
              _ (assert writer (str "No writer for " ftype))
              fend (if flookup
                     (let [value (if (= :INTO fname)
                                   (vec
                                     (for [fname (map second (:list flookup))
                                           :when (contains? msg-map fname)]
                                       [fname (get msg-map fname)]))
                                   (get msg-map fname))]
                       (writer buf value offset writers flookup))
                     (do (writer buf (get msg-map fname fdefault) offset)
                         (+ flen offset)))]
          (recur fields fend))))))

