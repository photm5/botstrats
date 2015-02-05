#!/bin/bash

scripts_dir=$(cd $(dirname $0); pwd)
data_dir=$1

source $scripts_dir/../utils.sh

function handle ()
{
    cd $data_dir
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

function handle_line ()
{
    receive
    handle $line
}

function main ()
{
    mkdir -p $data_dir/robots
    supervisor_uuid=$(uuidgen)

    send $(uuidgen) welcome $supervisor_uuid

    while true
    do
        handle_line
    done
}

main
