#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
port=2047
queue_dir=/tmp/botstrats/visualizer_queue

mkdir -p $queue_dir

cd $scripts_dir

./visualizer.lua $queue_dir &

ncat --keep-open -l $port |
while read line
do
    echo $line > $queue_dir/$(uuidgen)
done
