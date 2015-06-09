#!/usr/bin/env bash

function abs ()
{
    if [[ $1 -lt 0 ]]
    then
        echo $((- $1))
    else
        echo $1
    fi
}

function flush_message_queue ()
{
    cd $1
    if [[ $(ls -1 | wc -l) == 0 ]]
    then
        return 1
    fi
    while true
    do
        file_name=$(ls -1 | sort -n | sed q)
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
    if [[ $line ]]
    then
        out '<-' $line
    else
        sleep 0.1
        receive
    fi
}

function send ()
{
    out '->' $*
    >&1 echo $*
}

function kill_recursively ()
{
    while read process_id
    do
        kill_recursively $1 $process_id
    done < <(ps -o pid --no-headers --ppid $2)
    kill -s $1 $2
}

function kill_childs ()
{
    while read process_id
    do
        kill_recursively $1 $process_id
    done < <(jobs -p -r)
}
