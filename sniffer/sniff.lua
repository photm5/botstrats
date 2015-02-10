#!/bin/env lua

cjson = require 'cjson'

function append ( table_a, element )
    new_table = {}
    new_table = { table.unpack ( table_a ) }
    table.insert ( new_table, element )
    return new_table
end

function curry ( fun, num_args, args_list )
    print_list = args_list or {}
    if num_args == 1 then
        return function ( x )
            if args_list then
                return fun ( table.unpack ( append ( args_list, x ) ) )
            else
                return fun ( x )
            end
        end
    else
        return function ( x )
            if args_list then
                return curry ( fun, num_args - 1, append ( args_list, x ) )
            else
                return curry ( fun, num_args - 1, { x } )
            end
        end
    end
end

function repeated_find ( str, pattern, num )
    start_index = 0
    for i = 1, num do
        start_index, end_index = string.find ( str, pattern, start_index + 1 )
    end
    return start_index, end_index
end

-- <uuid> result scan <uuid> success ...

size = 500
ticks = 100000

in_stream = true
while true do
    line = io.read ()
    if not line then break end
    line_find = curry ( repeated_find, 3 ) ( line ) ( ' ' )
    function line_field ( num )
        return string.sub ( line, line_find ( num - 1 ) + 1, line_find ( num ) - 1 )
    end
    if line_field ( 2 ) == 'result' and line_field ( 3 ) == 'scan' then
        content = string.sub ( line, line_find ( 4 ) + 1, -1 )
        if content ~= 'success begin_of_stream' and content ~= 'end_of_stream' then
            robot = cjson.decode ( content )
            print ( table.concat ( { robot.uuid, '../resources/'..robot.type..':'..robot.status..'.png', size, size, robot.x * size, robot.y * -size, ticks }, ' ' ) )
        end
    end
end
