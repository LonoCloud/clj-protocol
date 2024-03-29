# Example programs

The project includes five working examples:

* A DHCP client
* A DHCP pool server
* A DHCP mac2ip server
* An ICMP/ping client
* A pcap file parser

Build the examples:

```
npx shadow-cljs compile simple-client pool-server mac2ip-server ping-client read-pcap
```

## Usage:

* **DHCP simple client** - Run a DHCP client on eth0. Listening on
  port 68 requires elevated permissions. WARNING: this will attempt to
  update your IP address on eth0 if it receives a successful response
  from a server.

  ```
  sudo node ./build/simple-client.js eth0
  ```

* **DHCP pool server** - Run a DHCP server on eth0 that allocates from
  a pool and stores the leases in a JSON file. This will listen for
  DHCP DISCOVER/REQUESTS and assign addresses from the pool.

  ```
  sudo node ./build/pool-server.js eth0
  ```

* **DHCP mac2ip server** - Run a DHCP server on eth0 that calculates
  the IP assignment based on the client's MAC address. The MAC to IP
  mappings are defined in a config file.

  ```
  sudo node ./build/mac2ip-server.js --if-name eth0 --config-file mac2ip.json
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
