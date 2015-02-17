socket = require 'socket'

utils = require 'utils'
config = require 'config'

local server = assert ( socket.bind ( config.host, config.port ) )
server:settimeout ( 0 )

return {
    clients = utils.signal (),
    poll = function ( self )
        local socket = server:accept ()
        if socket then
            local client = {}
            client.socket = socket
            client.id = utils.uuidgen ()
            client.receive = utils.signal ()
            client.send = utils.signal ()
            client.send:subscribe ( function ( str )
                client.socket:send ( str .. '\n' )
            end )
            self.clients ( client )
        end
    end,
}
