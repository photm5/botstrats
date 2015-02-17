-- request_id is optional
return function ( supervisor, desc, request_id )
    local robot = {}
    robot.type = desc.type
    robot.id = desc.id or utils.uuidgen ()
    robot.pos = desc.pos or lib.vector ( math.random ( -100, 100 ), math.random ( -100, 100 ) )
    robot.supervisor = supervisor.id
    if robot.type == 'headquarters' then
        robot.status = 'idle'
    else
        robot.status = 'off'
    end
    robots [ robot.id ] = robot
    supervisor.send_message ( { id = request_id, type = 'spawn', robot.type, robot.id } )
    return robot
end
