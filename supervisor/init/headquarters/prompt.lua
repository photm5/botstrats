require 'base'

actions = io.open ( 'actions', 'a' )
results = io.open ( 'results', 'r' )

function write_action ( action )
    print ( '-> ' .. action )
    actions:write ( action .. '\n' )
    actions:flush ()
end

function read_result ()
    local result = results:read ()
    print ( '<- ' .. result )
    return result
end
