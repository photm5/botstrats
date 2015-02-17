local directions = {
    north = lib.vector ( 0, 1 ),
    east  = lib.vector ( 1, 0 ),
    south = lib.vector ( 0, -1 ),
    west  = lib.vector ( -1, 0 ),
}

return function ( request, result )
    return {
        costs = {
            engineer = 1,
            specialist = 5,
        },
        cost = function ( self )
            return self.costs [ request.from.type ]
        end,
        parse = function ( self )
            request.direction = request.args [ 1 ]
        end,
        is_allowed = function ( self )
            return self.costs [ request.from.type ]
        end,
        check = function ( self )
            if not directions [ request.direction ] then
                result ( 'failure', 'no such direction' )
                return false
            end
            request.target = request.from.pos + directions [ request.direction ]
            if lib.collisions ( request.target ) then
                result ( 'failure', 'target occupied' )
                return false
            end
        end,
        execute = function ( self )
            request.from.pos = request.target
            result ( 'success' )
        end
    }
end
