#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
port=2000

data_dir=/tmp/botstrats/server

rm -rf $data_dir

ncat --keep-open --listen $port --sh-exec "$scripts_dir/handle_supervisor.sh $data_dir"
