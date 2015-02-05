#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
data_dir=$1

source $scripts_dir/../utils.sh

function enter_storage ()
{
    mkdir -p $data_dir/robots
    cd $data_dir
}

function handle_line ()
{
    enter_storage
    uuid=$1
    shift
    case $1 in
        spawn)
            out $supervisor_uuid wants to spawn a $2
            hq_uuid=$(uuidgen)
            send $uuid spawn $2 $hq_uuid
            mkdir robots/$hq_uuid
            cd robots/$hq_uuid
            echo $supervisor_uuid > supervisor
            echo $2 > type
            ;;
        action)
            ;;
        quit)
            out Quit by client.
            send $uuid quit requested
            exit 0
            ;;
        *)
            send $uuid invalid $*
            ;;
    esac
}

function main ()
{
    supervisor_uuid=$(uuidgen)

    send $(uuidgen) welcome $supervisor_uuid

    while true
    do
        receive
        handle_line $line
    done
}

main
