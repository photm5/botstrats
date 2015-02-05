#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
data_dir=$1
uuid=$(uuidgen)

source $scripts_dir/../utils.sh

echo WELCOME $uuid

while true
do
    read -r line
    out line received: $line
    case $line in
        ACTION*)
            ;;
        PING*)
            echo $line | sed s/I/O/
            ;;
        QUIT*)
            out Quit by client.
            echo QUIT REQUESTED
            break
            ;;
        *)
            echo INVALID $line
            out '(invalid)'
            ;;
    esac
done
