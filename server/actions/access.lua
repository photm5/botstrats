return function ( request, result )
    return {
        allowed_types = {
            factory = true,
            headquarters = true,
            specialist = true,
        },
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
        end,
        cost = function ( self )
            return 10
        end,
        range = function ( self )
            return 5
        end,
        execute = function ( self )
            if lib.distance ( request.from.pos, request.target.pos ) > self:range () then
                result ( 'failure', 'target too far away' )
            else
                request.supervisor.send_message ( {
                    id = request.id,
                    type = 'access',
                    request.target.id,
                    request.from.id,
                    request.target.supervisor
                } )
                -- supervisor sends result
            end
        end
    }
end
