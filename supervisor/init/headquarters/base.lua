cjson = require 'cjson'

function read_result_stream ()
    local result_list = {}
    while true do
        local result = read_result ()
        if result == 'success begin_of_stream' then
            -- ignore
        elseif result == 'end_of_stream' then
            break
        else
            table.insert ( result_list, result )
        end
    end
    return result_list
end

function result_split ( result )
    local i = string.find ( result, ' ' )
    if i then
        return string.sub ( result, 1, i - 1 ), string.sub ( result, i + 1 )
    else
        return i
    end
end

function result ()
    local result = read_result ()
    if result == 'success begin_of_stream' then
        return read_result_stream ()
    else
        return result_split ( result )
    end
end

function do_action ( action )
    write_action ( action )
    return result ()
end

function scan ()
    write_action ( 'scan' )
    local raw_robots = read_result_stream ()
    local robots = {}
    for k, raw_robot in pairs ( raw_robots ) do
        robots [ k ] = cjson.decode ( raw_robot )
    end
    return robots
end

function query ()
    local _, res = do_action ( 'query' )
    return cjson.decode ( res )
end

function copy ( filename, target, target_filename )
    target_filename = target_filename or filename

    if string.sub ( target, 1, 1 ) == '/' then
        status, path = 'success', target
    else
        status, path = do_action ( 'access ' .. target )
    end

    if status == 'success' then
        return os.execute ( 'cp ' .. filename .. ' ' .. path .. target_filename )
    else
        return false
    end
end

function mass_copy ( target, tasks )
    status, path = do_action ( 'access ' .. target )
    if status == 'success' then
        success = true
        for _, task in pairs ( tasks ) do
            if type ( task ) == 'table' then
                success = success and copy ( task [ 1 ], path, task [ 2 ] )
            else
                success = success and copy ( task, path, task )
            end
        end
        return success
    else
        return false
    end
end
