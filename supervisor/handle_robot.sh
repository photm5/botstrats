#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
data_dir=$1
robot_uuid=$2

source $scripts_dir/../utils.sh

cd $data_dir/robots/$2

robot_type=$(cat type)

function send_action ()
{
    cd $data_dir/message_queue
    uuid=$(uuidgen)
    action_type=$1
    shift
    echo "$uuid action $action_type $robot_uuid $*" > $uuid
}

function handle ()
{
    send_action $*
}

function handle_line ()
{
    receive
    handle $line
}

function handle_robot ()
{
    while true
    do
        if read -t 0
        then
            handle_line
        else
            flush_message_queue $data_dir/robots/$robot_uuid/results
        fi
        sleep 0.001
    done
}

function main ()
{
    if [[ -d $scripts_dir/init/$robot_type ]]
    then
        rsync $scripts_dir/init/$robot_type/ drive/ -a -q
    fi

    mkfifo fifo
    cd drive/

    cat ../fifo | ./init | handle_robot > ../fifo
}

main
