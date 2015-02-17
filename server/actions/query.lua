cjson = require 'cjson'

return function ( request, result )
    return {
        execute = function ( self )
            local res = {
                x = request.from.pos.x,
                y = request.from.pos.y,
                type = request.from.type,
                uuid = request.from.id,
            }
            result ( 'success', cjson.encode ( res ) )
        end
    }
end
