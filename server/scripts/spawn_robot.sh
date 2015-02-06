#!/bin/bash

robot_type=$1

robot_uuid=$(uuidgen)

send $uuid spawn $robot_type $robot_uuid

mkdir robots/$robot_uuid
cd robots/$robot_uuid

echo $supervisor_uuid > supervisor
echo $robot_type > type
