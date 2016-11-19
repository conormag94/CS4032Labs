#!/bin/sh

if [ $# -ne 1 ]; then
    echo $0: usage: start.sh node_ip port
    exit 1
fi

ip=$1
port=$2
python3 server.py $ip $port
