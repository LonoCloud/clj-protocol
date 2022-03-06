;; Copyright (c) 2021, Viasat, Inc
;; Licensed under EPL 2.0

(ns protocol.header
  "Read and write header data.

  Reader functions take `[buf start ctx]` and return `[end value]`
  where `end` is the offset in `buf` after the read value(s).

  Writer functions take `[buf msg-map start ctx]` and return `end`
  where `end` is the offset in `buf` after the written value(s)."

  (:require [protocol.platform :as plat]))

(def DEFAULT-BUF-SIZE 1500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compound reader and writer functions

(defn read-header
  "Reads/decodes `buf` starting at `start` based on `(:spec ctx)`
  using reader functions in `(:readers ctx)`.

  ## Parameters:
  * `buf`: is the buffer to read from
  * `start`: start reading from buf at this offset
  * `ctx`: a map of additional parsing context:
      * `readers`: map of types to reader functions
          * reader functions take [buf start end ctx]
      * `spec`: sequence of [name, type, extra-ctx]
         * `name`: field key used in returned map.
         * `type`: type to lookup in readers map
         * `extra-ctx`: extra context merged into ctx before reading:
             * `default`: ignored for reading
             * `length`:
                 * number: the number of bytes from the start of field (offset)
                 * field: the name of a previously read field that contains
                   number of bytes from start of the field (offset)
                 * defaults to length of remaining bytes in current context
      * `msg-map`: map of values previously read"
  [buf start {:keys [readers spec length] :as ctx}]
  (loop [fields spec
         offset start
         length (or length (- (plat/buf-len buf) offset))
         msg-map {}]
    (if (or (empty? fields) (>= offset (plat/buf-len buf)))
      [offset msg-map]
      (let [[[fname ftype fctx] & fields] fields
            flength (:length fctx)
            flength (or (get msg-map flength) flength length
                        (- (plat/buf-len buf) offset))
            ;;_ (prn :rh 0 :fname fname :ftype ftype :offset offset :length length :flength flength)
            reader (readers ftype)
            _ (assert reader (str "No reader for " ftype))
            ctx (merge ctx
                       fctx
                       {:msg-map msg-map
                        :length flength})
            [fend value] (reader buf offset ctx)
            length (- length (- fend offset))
            msg-map (assoc msg-map fname value)]
        (recur fields fend length msg-map)))))

(defn write-header
  "Writes/encodes data in `msg-map` into `buf` starting at `start`
  based on `(:spec ctx)` using writer functions in `(:writers ctx)`.

  ## Parameters:
  - `buf`: is the buffer to write into (if nil then allocates
    a buffer of `DEFAULT-BUF-SIZE` bytes)
  - `msg-map`: map of field names -> field values to encode.
  - `start`: start writing at offset
  - `ctx`: a map of additional parsing context:
      - `writers` is a map of types to writer functions
        - writer functions that take [buf val start ctx]
      - `spec`: sequence of [name, type, lenth, default]
          - `name`: field key to lookup in msg-map.
          - `type`: type to lookup in writers map
          - `extra-ctx`: extra context merged into ctx before writing
              - `default`: if msg-map does not contain `name` then this is
                 used instead
              - `length`:
                  - number: the number of bytes from the start of field (offset)
                  - field: the name of a field in msg-map that contains
                    number of bytes from start of the field (offset)
      - `msg-map`: parent msg-map for compound writers"
  [buf msg-map start {:keys [writers spec] :as ctx}]
  (loop [fields spec
         offset start]
    (if (or (empty? fields) (>= offset (plat/buf-len buf)))
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
            fend (if length
                   (+ offset (or (get msg-map length) length))
                   fend)]
        (recur fields fend)))))

;;;

(defn read-header-full
  "Like [[read-header]] but returns only the read value."
  [buf start ctx]
  (second (read-header buf start ctx)))

(defn write-header-full
  "Like [[write-header]] but allocates a default sized buffer if not
  provided and returns the encoded buffer sliced to size of the
  actual written data."
  [buf msg-map start ctx]
  (let [buf (cond (not buf)     (plat/buf-alloc DEFAULT-BUF-SIZE)
                  (number? buf) (plat/buf-alloc buf)
                  :else         buf)
        end (write-header buf msg-map start ctx)]
    (plat/buf-slice buf 0 end)))

(def ^{:doc "Header readers"}
  readers {:header read-header})

(def ^{:doc "Header writers"}
  writers {:header write-header})
