#!/bin/bash

data_dir=data_dir/
port=2000

scripts_dir=$(cd $(dirname $0); pwd)

ncat -l $port -c "$scripts_dir/handle_connection.sh"
