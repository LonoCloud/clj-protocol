# Example programs

The project includes three (or four) working examples:

* A DHCP client and server
* An ICMP/ping client
* A pcap file parser

Build the examples:

```
npx shadow-cljs compile dhcp-client dhcp-server ping-client read-pcap
```

## Usage:

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
