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
    shift
    case $1 in
        welcome)
            out I am $2
            my_id=$2
            ;;
        spawn)
            out Spawning robot $2 $3
            mkdir robots/$3
            cd robots/$3
            echo $2 > type
            mkdir drive
            mkdir results
            if [[ $2 == headquarters ]]
            then
                start_robot $3
            fi
            ;;
        start)
            start_robot $2
            ;;
        result)
            action_type=$2
            robot_uuid=$3
            shift 3
            cd robots/$robot_uuid/results
            echo $* > $uuid
            ;;
    esac
}

function handle_line ()
{
    receive
    handle $line
}

function flush_message_queue ()
{
    cd $data_dir/message_queue
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
            flush_message_queue
        fi
    done
    send $(uuidgen) quit
}

main
