return function ( supervisor, message )
    lib.spawn ( supervisor, { type = message [ 1 ] }, message.id )
end
