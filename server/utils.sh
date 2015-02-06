#!/bin/bash

function source_script ()
{
    script_name=$1
    shift
    source $scripts_dir/$script_name
}
