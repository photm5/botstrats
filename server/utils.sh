#!/bin/bash

function distance ()
{
    a_x=$1
    a_y=$2
    b_x=$3
    b_y=$4

    d_x=$(abs $(($a_x - $b_x)))
    d_y=$(abs $(($a_y - $b_y)))

    distance=$(echo "d = math.sqrt($d_x * $d_x + $d_y * $d_y) print(d - d % 1)" | lua)
    unset {a,b}_{x,y}
}

function robot_distance ()
{
    a_x=$(cat $data_dir/robots/$1/position/x)
    a_y=$(cat $data_dir/robots/$1/position/y)

    b_x=$(cat $data_dir/robots/$2/position/x)
    b_y=$(cat $data_dir/robots/$2/position/y)

    distance $a_x $a_y $b_x $b_y
}

function source_script ()
{
    script_name=$1
    shift
    source $scripts_dir/$script_name
}
