return function ( request, result )
    return {
        allowed_types = {
            factory = true,
            headquarters = true,
            specialist = true,
        },
        cost = function ( self )
            return 10
        end,
        range = function ( self )
            return 5
        end,
        parse = function ( self )
            request.target = robots [ request.args [ 1 ] ]
        end,
        is_allowed = function ( self )
            return self.allowed_types [ request.from.type ]
        end,
        check = function ( self )
            if not request.target then
                result ( 'failure', 'target does not exist' )
                return false
            end
            if request.target.status ~= 'off' then
                result ( 'failure', 'target not off' )
                return false
            end
            if lib.distance ( request.from.pos, request.target.pos ) > self:range () then
                result ( 'failure', 'target too far away' )
                return false
            end
        end,
        execute = function ( self )
            request.target.supervisor = request.supervisor.id
            request.target.status = 'idle'
            request.supervisor.send_message ( {
                id = request.id,
                type = 'start',
                request.target.id
            } )
            result ( 'success' )
        end
    }
end
