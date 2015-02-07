#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)

host=localhost
port=2000

data_dir=$scripts_dir/data_dir/

rm $data_dir -r

ncat localhost $port --sh-exec "$scripts_dir/handle_server.sh $data_dir"
