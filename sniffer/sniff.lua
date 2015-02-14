#!/usr/bin/env lua

cjson = require 'cjson'

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

while true do
    line = io.read ()
    if not line then break end
    function line_find ( n )
        return repeated_find ( line, ' ', n )
    end
    function line_field ( num )
        return string.sub ( line, line_find ( num - 1 ) + 1, line_find ( num ) - 1 )
    end
    if line_field ( 2 ) == 'result' and line_field ( 3 ) == 'scan' then
        content = string.sub ( line, line_find ( 4 ) + 1, -1 )
        if content ~= 'success begin_of_stream' and content ~= 'end_of_stream' then
            robot = cjson.decode ( content )
            print ( table.concat ( { robot.uuid, '../resources/'..robot.type..':'..robot.status..'.png', size, size, robot.x * size, robot.y * -size, ticks }, ' ' ) )
            io.stdout:flush ()
        end
    end
end
