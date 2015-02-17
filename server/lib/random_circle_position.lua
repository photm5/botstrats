function round ( num )
    if num % 1 >= .5 then
        away_from_0 = true
    else
        away_from_0 = false
    end
    if away_from_0 and num > 0 or
        not away_from_0 and num < 0 then
        return math.ceil ( num )
    else
        return math.floor ( num )
    end
end
return function ( radius )
    local angle = math.random () * 2 * math.pi
    local x = round ( math.cos ( angle ) * math.random () * radius )
    local y = round ( math.sin ( angle ) * math.random () * radius )
    return lib.vector ( x, y )
end
