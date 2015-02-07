#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)

host=localhost
port=2001

data_dir=$scripts_dir/data_dir/

rm $data_dir -r 2>/dev/null

ncat localhost $port --sh-exec "$scripts_dir/handle_server.sh $data_dir"
