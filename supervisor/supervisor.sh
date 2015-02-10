#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)

host=localhost
port=2001

data_dir=/tmp/botstrats/supervisor

rm -rf $data_dir

ncat localhost $port --sh-exec "$scripts_dir/handle_server.sh $data_dir"
