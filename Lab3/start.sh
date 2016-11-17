#!/bin/sh

if [ $# -ne 1 ]; then
    echo $0: usage: start.sh port
    exit 1
fi

port=$1
python3 server.py $port
