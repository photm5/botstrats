#!/bin/bash

radius=$1

angle=$(echo "math.randomseed($RANDOM) print(math.random() * 2 * math.pi)" | lua)

x=$(echo "print(math.cos($angle)*$radius)" | lua)
y=$(echo "print(math.sin($angle)*$radius)" | lua)

# truncate
x=$(echo "print($x - $x % 1)" | lua)
y=$(echo "print($y - $y % 1)" | lua)

unset radius angle
