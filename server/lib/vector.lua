local metatable = {
    __add = function ( a, b )
        return new_vector ( a.x + b.x, a.y + b.y )
    end,
    __sub = function ( a, b )
        return new_vector ( a.x - b.x, a.y - b.y )
    end,
    __mul = function ( vect, num )
        return new_vector ( vect.x * num, vect.y * num )
    end,
    __div = function ( vect, num )
        return new_vector ( vect.x / num, vect.y / num )
    end,
    __unm = function ( vect )
        return new_vector ( -vect.x, -vect.y )
    end,
    __len = function ( vect )
        return math.sqrt ( vect.x ^ 2 + vect.y ^ 2 )
    end,
    __eq = function ( a, b )
        return a.x == b.x and a.y == b.y
    end,
}
function new_vector ( ... )
    local args = { ... }
    local coords
    if #args == 1 then
        coords = args [ 1 ]
    else
        coords = { x = args [ 1 ], y = args [ 2 ] }
    end
    setmetatable ( coords, metatable )
    return coords
end

return new_vector
