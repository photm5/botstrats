#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
data_dir=$1

source $scripts_dir/../utils.sh

function start_robot ()
{
    out Starting robot $1
    >&2 $scripts_dir/handle_robot.sh $data_dir $1 &
}

function handle ()
{
    cd $data_dir
    uuid=$1
    command_type=$2
    command_script=$scripts_dir/commands/$command_type
    if [[ -e $command_script ]]
    then
        shift 2
        source $command_script
    else
        shift 1
        send $uuid invalid $*
    fi
}

function handle_line ()
{
    receive
    handle $line
}

function main ()
{
    mkdir -p $data_dir/{robots,message_queue}

    handle_line
    send $(uuidgen) spawn headquarters
    handle_line
    while true
    do
        if read -t 0
        then
            handle_line
        else
            flush_message_queue $data_dir/message_queue
        fi
        sleep 0.01
    done
}

main
