(ns protocol.header)

(def DEFAULT-BUF-SIZE 1500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compound reader and writer functions

(defn read-header
  "Takes [buf start end ctx] and parses 'buf' based on '(:spec ctx)'
  using reader functions in '(:readers ctx)'. Returns a map of parsed
  fields with metdata '{:protocol/end end}' (where end is offset after
  read).

  Parameters:
  - 'buf': is the js/Buffer to read from
  - 'start': start reading from buf at this offset
  - 'ctx': a map of additional parsing context:
    - 'readers': map of types to reader functions
      - reader functions take [buf start end ctx]
    - 'spec': sequence of [name, type, extra-ctx]
      - 'name': field key used in returned map.
      - 'type': type to lookup in readers map
      - 'extra-ctx': extra context merged into ctx before reading:
        - 'default': ignored for reading
        - 'length': length of field and can be:
          - number: the number of bytes from the start of field (offset)
          - ':*': the rest of the buffer
          - field: the name of a previously read field that contains
            number of bytes from start of the field (offset)
    - 'msg-map': map of values previously read
  "
  [buf start {:keys [readers spec] :as ctx}]
  (loop [fields spec
         offset start
         msg-map {}]
    (if (or (empty? fields) (>= offset (.-length buf)))
      [offset msg-map]
      (let [[[fname ftype fctx] & fields] fields
            {:keys [length]} fctx
            length (if (= :* length)
                     (- (.-length buf) offset)
                     (or (get msg-map length) length))
            reader (readers ftype)
            _ (assert reader (str "No reader for " ftype))
            ctx (merge ctx fctx {:msg-map msg-map}
                       (when length {:length length}))
            [fend value] (reader buf offset ctx)
            ;;_ (prn :rh :fname fname :ftype ftype :offset offset :length length :fend fend :value value :reader reader :fields-cnt (+ 1 (count fields)))
            fend (if length (+ offset length) fend)
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
      - 'extra-ctx': extra context merged into ctx before writing
        - 'default': if msg-map does not contain 'name' then this is
           used instead
        - 'length':
          - number: the number of bytes from the start of field (offset)
          - ':*': the rest of the buffer
          - field: the name of a field in msg-map that contains
            number of bytes from start of the field (offset)
    - 'msg-map': parent msg-map for compound writers
  "
  [buf msg-map start {:keys [writers spec] :as ctx}]
  (loop [fields spec
         offset start]
    (if (or (empty? fields) (>= offset (.-length buf)))
      offset
      (let [[[fname ftype fctx] & fields] fields
            {:keys [length]} fctx
            writer (writers ftype)
            _ (assert writer (str "No writer for " ftype))
            value (get msg-map fname (get fctx :default))
            ;;_ (prn :wh :start start :fname fname :ftype ftype :length length :value value)
            ctx (merge ctx fctx {:msg-map msg-map})
            ;; For fixed sized fields, ignore bytes written
            fend (writer buf value offset ctx)
            fend (cond (= :* length) fend
                       length (+ offset (or (get msg-map length) length))
                       :else fend)]
        (recur fields fend)))))

;;;

(defn read-header-full
  "Like read-header but returns only the read value."
  [buf start ctx]
  (second (read-header buf start ctx)))

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
