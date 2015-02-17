return function ( supervisor, message )
    local robot_id = message [ 1 ]
    robots [ robot_id ].status = 'off'
end
