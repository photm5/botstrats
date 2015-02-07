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

-- <uuid> result scan <uuid> success <json>

while true do
    line = io.read ()
    if not line then break end
    line_find = curry ( repeated_find, 3 ) ( line ) ( ' ' )
    function line_field ( num )
        return string.sub ( line, line_find ( num - 1 ) + 1, line_find ( num ) - 1 )
    end
    if line_field ( 2 ) == 'result' and line_field ( 3 ) == 'scan' then
        json_string = string.sub ( line, line_find ( 5 ) + 1, -1 )
        scan_results = cjson.decode ( json_string )
        for _, robot in pairs ( scan_results ) do
            print ( table.concat ( { robot.uuid, 'res/'..robot.type..'.png', 100, 100, robot.x * 100, robot.y * 100, 100000 }, ' ' ) )
        end
    end
end
