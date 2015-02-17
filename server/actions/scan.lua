cjson = require 'cjson'

return function ( request, result )
    return {
        cost = function ( self )
            if request.from.type == 'headquarters' then
                return 0
            else
                return 10
            end
        end,
        range = function ( self )
            if request.from.type == 'headquarters' then
                return 100
            else
                return 10
            end
        end,
        execute = function ( self )
            result ( 'success', 'begin_of_stream' )
            for _, robot in pairs ( robots ) do
                if lib.distance ( request.from.pos, robot.pos ) < self:range () then
                    result ( cjson.encode ( {
                        uuid = robot.id,
                        x = robot.pos.x,
                        y = robot.pos.y,
                        type = robot.type,
                        status = robot.status
                    } ) )
                end
            end
            result ( 'end_of_stream' )
        end
    }
end
