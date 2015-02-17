function require_tree ( name )
    local tree = {}
    for path in io.popen ( 'find ' .. name .. ' -type f' ):lines () do
        local module_name = string.gsub ( path, '%.lua$', '' )
        module_name = string.gsub ( module_name, '/', '.' )
        local module_parts = {}
        for part in string.gmatch ( module_name, '[^%.]+' ) do
            table.insert ( module_parts, part )
        end
        table.remove ( module_parts, 1 )
        local current_node = tree
        for i, part in pairs ( module_parts ) do
            if i < #module_parts then
                current_node [ part ] = current_node [ part ] or {}
                current_node = current_node [ part ]
            else
                current_node [ part ] = require ( module_name )
            end
        end
    end
    return tree
end

function uuidgen ()
    local handle = io.popen ( 'uuidgen' )
    local uuid = handle:read ()
    handle:close ()
    return uuid
end

function build_message ( message )
    if type ( message ) == 'string' then
        local raw = message
        message = {}
        message.raw = raw
        for part in string.gmatch ( message.raw, '[^ ]+' ) do
            table.insert ( message, part )
        end
        message.id = message [ 1 ]
        message.type = message [ 2 ]
        -- remove id and type, so that only parameters remain
        table.remove ( message, 1 )
        table.remove ( message, 1 )
    elseif type ( message ) == 'table' then
        message.id = message.id or uuidgen ()
        message.raw = table.concat ( { message.id, message.type, table.unpack ( message ) }, ' ' )
    end
    return message
end

function signal ()
    local signal = {
        handlers = {},
        subscribe = function ( self, handler, identifier )
            identifier = identifier or #self.handlers + 1
            self.handlers [ identifier ] = handler
        end,
        unsubscribe = function ( self, identifier )
            self.handlers [ identifier ] = nil
        end
    }
    setmetatable ( signal, {
        __call = function ( self, ... )
            for _, handler in pairs ( self.handlers ) do
                handler ( ... )
            end
        end
    } )
    return signal
end

return {
    require_tree = require_tree,
    uuidgen = uuidgen,
    build_message = build_message,
    signal = signal
}
