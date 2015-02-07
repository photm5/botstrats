#!/bin/bash

robot_type=$1

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

    offset_x=$(($RANDOM % 10 - 5))
    offset_y=$(($RANDOM % 10 - 5))

    pos_x=$(($factory_pos_x + $offset_x))
    pos_y=$(($factory_pos_y + $offset_y))
}

cd $data_dir/robots

while true
do
    gen_position
    while read other_uuid
    do
        [[ $pos_x == $(cat $other_uuid/position/x) &&
        $pos_y == $(cat $other_uuid/position/y) ]] && continue 2
    done < <(ls -1)
    break
done

send $uuid spawn $robot_type $robot_uuid $pos_x $pos_y

mkdir $robot_uuid
cd $robot_uuid

echo $supervisor_uuid > supervisor
echo $robot_type > type

mkdir position
echo $pos_x > position/x
echo $pos_y > position/y
