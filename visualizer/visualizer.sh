#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
port=2047

cd $scripts_dir

./visualizer.lua &

ncat --keep-open -l $port |
while read line
do
    echo $line > queue/$(uuidgen)
done
