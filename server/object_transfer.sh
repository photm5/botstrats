#!/bin/bash

function gen_object_path ()
{
    if [[ $1 =~ ([0-9a-f]{2})([0-9a-f]{38}) ]]
    then
        echo data_dir/objects/${BASH_REMATCH[1]}/${BASH_REMATCH[2]}
        return 0
    else
        return 1
    fi
}

function send ()
{
    sha=$2
    object_path="$(gen_object_path $2)"
    if [[ $? == 1 ]]
    then
        echo SHA1 INVALID
        return 1
    fi
    if [[ ! -e $object_path ]]
    then
        echo OBJECT NOT AVAILABLE
        return 1
    else
        echo SEND $sha $(cat $object_path | wc -l)
        cat $object_path
    fi
}

function receive ()
{
    remote_sha=$2
    length=${3-0}
    if [[ $length == 0 ]]
    then
        echo LENGTH INVALID
        return 1
    fi
    object_path="$(gen_object_path $2)"
    if [[ $? == 1 ]]
    then
        echo SHA1 INVALID
        return 1
    fi
    mkdir -p $(dirname $object_path)
    sed ${length}q >> $object_path
    real_sha=$(sha1sum $object_path | cut -d ' ' -f 1)
    if [[ $remote_sha != $real_sha ]]
    then
        echo SHA1 INVALID
    else
        echo RECEIVE SUCCESSFUL
    fi
}
