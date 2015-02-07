#!/bin/bash

visualizer_port=2047

scripts_dir=$(cd $(dirname $0); pwd)

mkfifo log 2>/dev/null

cat to_server >/dev/null &
cat to_client | ./sniff.lua > log &

cat log | nc localhost $visualizer_port
