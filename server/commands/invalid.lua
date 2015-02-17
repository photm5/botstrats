return function ( supervisor, message )
    supervisor.send_message ( { id = message.id, type = 'invalid' } )
end
