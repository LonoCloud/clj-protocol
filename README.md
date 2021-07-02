# clj-protocol: Declarative Protocols and Binary Formats

## Tutorial

## API Reference

### Header Defintions

### Simple Readers / Writers

* Numeric types
* Address types
* String
* Raw
* Buffer

### Compound Readers / Writers

* bitfield
* lookup
* repeat
* loop
* choice
* tlvs

## TODO
- add signed int value types ?
- :lookup -> :tlv-lookup ?

## Running / testing on NixOS

Run the following on NixOS to get the required node modules (pcap and
raw-socket) installed to be able to run the ping and and dhcp
commands:

```
rm -r node_modules
nix-shell -p libpcap --run 'npm install'
```

