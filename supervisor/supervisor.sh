#!/usr/bin/env bash

scripts_dir=$(cd $(dirname $0); pwd)

host=localhost
port=2001

data_dir=/tmp/botstrats/supervisor

rm -rf "$data_dir"

mkdir -p "$data_dir"

mkfifo "$data_dir/fifo"
cat "$data_dir/fifo" &

nc localhost $port --exec="$scripts_dir/handle_server.sh $data_dir 2>$data_dir/fifo"
