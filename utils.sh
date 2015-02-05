#!/bin/bash

function out ()
{
    >&2 echo $*
}

function receive ()
{
    read -r line
    out '<-' $line
}

function send ()
{
    out '->' $*
    >&1 echo $*
}
