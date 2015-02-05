#!/bin/bash

host=localhost
port=2000

scripts_dir=$(cd $(dirname $0); pwd)

ncat localhost 2000 --sh-exec "$scripts_dir/handle_server.sh $scripts_dir/data_dir/"
