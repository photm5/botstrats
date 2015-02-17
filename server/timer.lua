socket = require 'socket'

config = require 'config'

return function ( pollers )
    return {
        callbacks = {},
        loop = function ( self )
            while true do
                local sleep = true
                for _, poller in pairs ( pollers ) do
                    local res = poller ()
                    if res ~= nil then
                        sleep = sleep and res
                    end
                end
                for i, callback in pairs ( self.callbacks ) do
                    if callback.at < socket.gettime () then
                        sleep = false
                        callback.callback ()
                        self.callbacks [ i ] = nil
                    end
                end
                if sleep then
                    socket.sleep ( config.sleeptime )
                end
            end
        end,
        register = function ( self, delay, callback )
            table.insert ( self.callbacks, { callback = callback, at = socket.gettime () + delay } )
        end
    }
end
