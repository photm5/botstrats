#!/bin/bash

function robot_distance ()
{
    a_x=$(cat $data_dir/robots/$1/position/x)
    a_y=$(cat $data_dir/robots/$1/position/y)

    b_x=$(cat $data_dir/robots/$2/position/x)
    b_y=$(cat $data_dir/robots/$2/position/y)

    d_x=$(abs $(($a_x - $b_x)))
    d_y=$(abs $(($a_y - $b_y)))

    distance=$(echo "sqrt ( $d_x ^ 2 + $d_y ^ 2 )" | bc)
    unset {a,b}_{x,y}
}

function source_script ()
{
    script_name=$1
    shift
    source $scripts_dir/$script_name
}
