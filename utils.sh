#!/bin/bash

function flush_message_queue ()
{
    cd $1
    while true
    do
        file_name=$(find -type f | sed q)
        if [[ $file_name != '' ]]
        then
            send $(cat $file_name)
            rm $file_name
        else
            break
        fi
    done
}

function out ()
{
    >&2 echo $*
}

function receive ()
{
    read -r line
    out '<-' $line
}

function send ()
{
    out '->' $*
    >&1 echo $*
}
