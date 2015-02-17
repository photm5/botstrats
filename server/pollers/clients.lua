utils = require 'utils'

return {
    clients = {},
    poll = function ( self )
        for _, client in pairs ( self.clients ) do
            line, err = client.socket:receive ()
            if line then
                client.receive ( line )
            end
            if err == 'closed' then
                self:remove ( client.id )
                break
            end
        end
    end,
    remove = function ( self, id )
        print ( 'removing client ' .. id )
        self.clients [ id ] = nil
    end,
    add = function ( self, client )
        print ( 'adding client ' .. client.id )
        self.clients [ client.id ] = client
        client.socket:settimeout ( 0 )
    end,
}
