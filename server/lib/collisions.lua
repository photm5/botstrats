return function ( ... )
    local pos = lib.vector ( ... )
    for _, robot in pairs ( robots ) do
        if pos == robot.pos then
            return true
        end
    end
end
