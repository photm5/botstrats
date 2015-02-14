#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)

listen_port=2001
connect_port=2000

cd $scripts_dir

mkfifo to_{client,server} 2>/dev/null

ncat --keep-open --listen $listen_port \
    --sh-exec "$scripts_dir/sniff.sh & tee to_server | nc localhost $connect_port | tee to_client"
