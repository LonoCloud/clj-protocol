#!/bin/bash

set -e

wait_for_if() {
  local if=$1
  while true; do
    ip link show $if >/dev/null 2>&1 && return 0
    sleep 1
  done
}

echo "Waiting for eth0 to appear"
wait_for_if eth0

#echo "Links:"
#ip -o link show
#echo "Addresses:"
#ip -o addr show

#echo "Starting tcpdump"
#tcpdump -enli eth0 &

# Give servers a chance to fully start
case "${*}" in
  *client*) sleep 2 ;;
esac

echo "Running: ${@}"
${@}
