# clj-protocol: Declarative Protocols and Binary Formats

clj-protocol enables declarative definition of network protocols and
binary formats.

Full API Documentation (generated with codox) is available
[here](https://lonocloud.github.io/clj-protocol/www/index.html).

clj-protocol is primarily designed for use in ClojureScript (due to
stronger host networking interfaces), however, the core libraries also
support Clojure (JVM).

## Quick Tutorial

[//]: # (This should be kept in sync with docs/tutorial.md)

Install dependencies and then start a REPL either using shadow-cljs or
lumo:

```
cd clj-protocol
npm install
npx shadow-cljs node-repl
  # OR
lumo -c src
```

If you are using Clojure (JVM) then you can use leiningen to start
a REPL:

```
cd clj-protocol
lein repl
```

Require some clj-protocol namespaces:

```
cljs.user=> (require '[protocol.platform :as platform])
cljs.user=> (require '[protocol.fields :as fields])
cljs.user=> (require '[protocol.header :as header])
```

Define binary data in a Buffer (or java.nio.ByteBuffer) that we will
read/parse:

```
user> (def buf (platform/buf-from [0x61 0x62 0x63 0x64]))
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
cljs.user=> (prn (platform/buf->vec buf2 0))
[0 0 10 0 0 16]
```


## Example Programs

[//]: # (This should be kept in sync with docs/examples.md)

The project includes three (or four) working examples:

* A DHCP client and server
* An ICMP/ping client
* A pcap file parser

Build the examples:

```
npx shadow-cljs compile dhcp-client dhcp-server ping-client read-pcap
```

### Usage:

* **DHCP client** - Run a DHCP client on eth0. Listening on port 68
  requires elevated permissions. WARNING: this will attempt to update
  your IP address on eth0 if it receives a successful response from
  a server.

  ```
  sudo node ./build/dhcp-client.js eth0
  ```

* **DHCP server** - Run a DHCP server on eth0 that allocates from
  a pool and stores the leases in a JSON file. This will listen for
  DHCP DISCOVER/REQUESTS and assign addresses from the pool.

  ```
  sudo node ./build/dhcp-server.js eth0
  ```

* **ICMP/ping client** - Use the ping client to demonstrate ICMP
  protocl reading/writing. Elevated permissions are required to
  send/receive ICMP packets.

  ```
  sudo node ./build/ping.js 8.8.8.8
  ```

* **pcap file parser** - Use the pcap file parser to print the header
  and records from the example pcap file.

  ```
  node ./build/read-pcap.js test/example.pcap
  ```

## Tests

Build and run the tests:

```
npx shadow-cljs compile test
node build/test.js
```

If you are using Clojure (JVM) then run tests using leiningen:

```
lein test
```

Use docker-compose and [conlink](https://github.com/LonoCloud/conlink)
to launch a self-contained network environment that runs the DHCP
client, server, and ping client.

```
cd test
docker-compose up --force-recreate --build
```


## Creating custom reader/writer functions

- Context should be passed to any internal reader/writer calls.

## API

All public functions in the `protocol` namespaces have docstrings that
describe the API.

You can use leiningen to generate codox documentation in `docs/www`
like this:

```
lein codox
```


## TODO
- add signed int value types ?
- :lookup -> :tlv-lookup ?

## Running / Testing on NixOS

Run the following on NixOS to get the required node modules (pcap and
raw-socket) installed to be able to run the ping and and dhcp
commands:

```
rm -r node_modules
nix-shell -p libpcap --run 'npm install'
```

## Copyright & License

This software is copyright Viasat, Inc and is released under the terms
of the Eclipse Public License version 2.0 (EPL.20). A copy of the
license is located at in the LICENSE file at the top of the
repository.
