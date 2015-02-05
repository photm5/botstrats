#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)

data_dir=$1
robot_uuid=$2

cd $data_dir/robots/$2

robot_type=$(cat type)

source $scripts_dir/../utils.sh

function send_action ()
{
    cd $data_dir/message_queue
    uuid=$(uuidgen)
    echo "$uuid $*" > $uuid
}

function handle ()
{
    out $*
    case $1 in
        spawn)
            robot_type=$2
            [[ $robot_type == '' ]] && return
            send_action spawn $robot_type
            ;;
    esac
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
        handle_line
    done
}

function main ()
{
    if [[ -e $scripts_dir/init/$robot_type ]]
    then
        cp $scripts_dir/init/$robot_type drive/init
        chmod +x drive/init
    fi

    mkfifo fifo
    cd drive/

    cat ../fifo | ./init | handle_robot > ../fifo

    rm ../fifo
}

main
