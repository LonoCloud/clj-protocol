- Install node 7.X

- Install lumo
  `npm install -g lumo-cljs`



- start mininet network
  `sudo python test/mininet-sshd.py`
  `sudo lsns`  # to get PIDs

- h1:
  `sudo nsenter -t PID-H1 -n`
  `./json_pool_server.sh h1-eth0`

- h2:
  `sudo nsenter -t PID-H2 -n`
  `sudo ./client.sh h2-eth0 10.0.0.1`
    # OR
  `sudo ./client.sh h2-eth0`
    # OR same as
  `sudo ./client.sh h2-eth0 10.255.255.255`

- cleanup mininet
  - exit nsenter's
  `sudo mn -c`