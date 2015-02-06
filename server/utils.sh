#!/bin/bash

function source_script ()
{
    script_name=$1
    shift
    out "source $scripts_dir/$script_name"
    source $scripts_dir/$script_name
}
