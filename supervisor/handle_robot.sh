#!/usr/bin/env bash

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
    while [[ $(jobs -r | wc -l) == 1 ]]
    do
        if read -t 0
        then
            handle_line
        else
            if flush_message_queue $data_dir/robots/$robot_uuid/results
            then
                : # results where found
            else
                sleep 0.1
            fi
        fi
    done
}

function main ()
{
    if [[ -d $scripts_dir/init/$robot_type ]]
    then
        rsync $scripts_dir/init/$robot_type/ drive/ -a -q
    fi

    rm -f from to
    mkfifo from to
    cd drive/

    ./init > ../from < ../to &
    handle_robot < ../from > ../to

    cd $data_dir/message_queue
    uuid=$(uuidgen)
    echo "$uuid robot_stopped $robot_uuid" > $uuid
}

main
