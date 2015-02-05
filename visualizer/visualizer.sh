#!/bin/bash

port=2047

./visualizer.lua &

ncat --keep-open -l $port |
while read line
do
    echo $line > queue/$(uuidgen)
done
