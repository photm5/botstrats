return function ( request, result )
    return {
        type_table = {
            engineer = {
                factory = 10,
            },
            factory = {
                engineer = 5,
            },
            headquarters = {
                engineer = 5,
                specialist = 10,
            }
        },
        parse = function ( self )
            request.spawned_type = request.args [ 1 ]
            if request.args [ 3 ] then
                request.pos = lib.vector ( request.args [ 2 ], request.args [ 3 ] )
            end
        end,
        cost = function ( self )
            return self.type_table [ request.from.type ] [ request.spawned_type ]
        end,
        range = function ( self )
            return 5
        end,
        is_allowed = function ( self )
            return self.type_table [ request.from.type ] and true
        end,
        check = function ( self )
            if not self.type_table [ request.from.type ] [ request.spawned_type ] then
                result ( 'failure', 'you may not spawn that type of robot' )
                return false
            end
            if request.pos then
                if not ( lib.distance ( request.from.pos, request.pos ) < self:range () ) then
                    result ( 'failure', 'the new position is too far away' )
                    return false
                end
                if lib.collisions ( request.pos ) then
                    result ( 'failure', 'the new position is occupied' )
                    return false
                end
            end
        end,
        execute = function ( self )
            local pos = request.pos
            if not request.pos then
                for i = 1, config.random_position_tries do
                    pos = lib.random_circle_position ( self:range () ) + request.from.pos
                    if not lib.collisions ( pos ) then
                        break
                    end
                end
                if lib.collisions ( pos ) then
                    result ( 'failure', 'failed to find a free spot' )
                    return
                end
            end
            local robot = lib.spawn ( request.supervisor, { pos = pos, type = request.spawned_type }, request.id )
            result ( 'success', robot.id )
        end
    }
end
