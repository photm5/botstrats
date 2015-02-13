#!/bin/bash

robot_type=$1

if [[ $3 ]]
then
    pos_x=$2
    pos_y=$3
else
    offset_radius=$2
fi

robot_uuid=$(uuidgen)

function gen_position ()
{
    if [[ $factory_uuid ]]
    then
        factory_pos_x=$(cat $factory_uuid/position/x)
        factory_pos_y=$(cat $factory_uuid/position/y)
    else
        factory_pos_x=$(($RANDOM % 200 - 100))
        factory_pos_y=$(($RANDOM % 200 - 100))
    fi

    source_script scripts/random_position_inside_circle.sh $offset_radius

    pos_x=$(($factory_pos_x + $x))
    pos_y=$(($factory_pos_y + $y))
}

cd $data_dir/robots

if [[ ! ( $pos_x && $pos_y ) ]]
then
    while true
    do
        gen_position
        [[ -f $data_dir/position_map/$pos_x:$pos_y ]] && continue
        break
    done
fi

send $uuid spawn $robot_type $robot_uuid

mkdir $robot_uuid
cd $robot_uuid

echo $supervisor_uuid > supervisor
echo $robot_type > type

mkdir position
echo $pos_x > position/x
echo $pos_y > position/y

mkdir -p $data_dir/position_map
echo $robot_uuid > $data_dir/position_map/$pos_x:$pos_y

if [[ $robot_type == headquarters ]]
then
    echo idle > status
else
    echo off > status
fi
