#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
data_dir=$scripts_dir/data_dir/
port=2000

rm $data_dir -r

ncat --keep-open --listen $port --sh-exec "$scripts_dir/handle_supervisor.sh $data_dir"
