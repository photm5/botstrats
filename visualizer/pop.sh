#!/bin/bash

cd queue
while true
do
    file_name=$(find -type f | sed q)
    if [[ $file_name != '' ]]
    then
        cat $file_name
        rm $file_name
    else
        break
    fi
done
