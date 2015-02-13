#!/bin/bash

radius=$1

angle=$(echo "math.randomseed($RANDOM) print(math.random() * 2 * math.pi)" | lua)

x=$(echo "math.randomseed($RANDOM) print(math.cos($angle)*math.random()*$radius)" | lua)
y=$(echo "math.randomseed($RANDOM) print(math.sin($angle)*math.random()*$radius)" | lua)

# truncate
x=$(echo "print($x - $x % 1)" | lua)
y=$(echo "print($y - $y % 1)" | lua)

unset radius angle
