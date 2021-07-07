# Quick Tutorial

[//]: # (This should be kept in sync with README.md)

Install dependencies and then start a REPL either using shadow-cljs or
lumo:

```
cd clj-protocol
npm install
npx shadow-cljs node-repl
  # OR
lumo -c src
```

Require some clj-protocol namespaces:

```
cljs.user=> (require '[protocol.fields :as fields])
cljs.user=> (require '[protocol.header :as header])
```

Define binary data in a Buffer that we will read/parse:

```
cljs.user=> (def buf (.from js/Buffer #js [0x61 0x62 0x63 0x64]))
```

Define a data format spec that specifies two 8-bit unsigned integers followed by
two 16-bit unsigned integers:

```
cljs.user=> (def spec1 [[:f1 :uint8] [:f2 :uint8] [:f3 :uint16]])
```

Use that spec and big-endian field readers to parse the buffer:

```
cljs.user=> (prn (header/read-header-full
                   buf 0 {:spec spec1 :readers fields/readers-BE}))
{:f1 97, :f2 98, :f3 25444}
```

Parse using the same spec but using little-endian field readers:

```
cljs.user=> (prn (header/read-header-full
                   buf 0 {:spec spec1 :readers fields/readers-LE}))
{:f1 97, :f2 98, :f3 25699}
```

Define an alternate data format spec that specifies a single fixed
length (4 byte) UTF-8 encoded string and then use that to parse the
buffer:

```
cljs.user=> (def spec2 [[:s1 :utf8 {:length 4}]])
cljs.user=> (prn (header/read-header-full
                   buf 0 {:spec spec2 :readers fields/readers-BE}))
{:s1 "abcd"}
```

Define an alternate spec that species a 4-byte bitfield containing
three fields: a 7-bit unsigned integer field, a 1-bit boolean field,
and a 24-bit unsigned integer field.

```
cljs.user=> (def spec3 [[:f1 :bitfield {:length 4
                                        :spec [[:b1 :int   7]
                                               [:b2 :bool  1]
                                               [:b3 :int  24]]}]])
cljs.user=> (prn (header/read-header-full
                   buf 0 {:spec spec3 :readers fields/readers-BE}))
{:f1 {:b1 48, :b2 true, :b3 6447972}}
```

Use the same bitfield spec to encode different values into a buffer
starting at offset 2:

```
cljs.user=> (def msg {:f1 {:b1 5, :b2 false, :b3 16}})
cljs.user=> (def buf2 (header/write-header-full
                        nil msg 2 {:spec spec3 :writers fields/writers-BE}))
cljs.user=> (prn (vec buf2))
[0 0 10 0 0 16]
```

