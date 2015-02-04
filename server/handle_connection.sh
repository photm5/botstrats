#!/bin/bash

data_dir=$1
uuid=$(uuidgen)

source utils.sh
source object_transfer.sh

echo WELCOME $uuid

while true
do
    read -r line
    out line received: $line
    case $line in
        REQUEST*)
            send $line
            ;;
        SEND*)
            receive $line
            ;;
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
