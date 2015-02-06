#!/bin/bash

robot_type=$1

robot_uuid=$(uuidgen)

if [[ $factory_uuid ]]
then
    factory_pos_x=$(cat robots/$factory_uuid/position/x)
    factory_pos_y=$(cat robots/$factory_uuid/position/y)
else
    factory_pos_x=$(($RANDOM % 500 - 250))
    factory_pos_y=$(($RANDOM % 500 - 250))
fi

offset_x=$(($RANDOM % 10 - 5))
offset_y=$(($RANDOM % 10 - 5))

pos_x=$(($factory_pos_x + $offset_x))
pos_y=$(($factory_pos_y + $offset_y))

send $uuid spawn $robot_type $robot_uuid $pos_x $pos_y

mkdir robots/$robot_uuid
cd robots/$robot_uuid

echo $supervisor_uuid > supervisor
echo $robot_type > type

mkdir position
echo $pos_x > position/x
echo $pos_y > position/y
