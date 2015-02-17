return function ( supervisor, message )
    local request = {}
    request.id = message.id
    request.supervisor = supervisor
    request.type = message [ 1 ]
    request.from_id = message [ 2 ]

    function result ( ... )
        supervisor.send_message ( { id = message.id, type = 'result', request.type, request.from_id, ... } )
    end

    request.from = robots [ request.from_id ]
    if not request.from then
        result ( 'failure', 'requester does not exist' )
        return
    end
    -- remove type and from, so that only args remain
    table.remove ( message, 1 )
    table.remove ( message, 1 )
    request.args = message

    if not actions [ request.type ] then
        result ( 'failure', 'no such action' )
        return
    end

    if request.from.status ~= 'idle' then
        result ( 'failure', 'requester not idle' )
        return
    end

    local handler = actions [ request.type ] ( request, result )
    handler.is_allowed = handler.is_allowed or function () return true end
    handler.check = handler.check or function () return true end
    handler.cost = handler.cost or function () return 0 end
    handler.parse = handler.parse or function () end

    handler:parse ()

    if not handler:is_allowed () then
        result ( 'failure', 'no such action for your type' )
        return
    end

    local allowed = handler:check ()
    if allowed == nil then
        allowed = true
    end

    if allowed then
        request.from.status = request.type
        timer:register ( handler:cost (), function ()
            handler:execute ()
            request.from.status = 'idle'
        end)
    end
end
