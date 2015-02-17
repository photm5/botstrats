#!/usr/bin/env lua

socket = require 'socket'

config = require 'config'
utils = require 'utils'

lib = utils.require_tree ( 'lib' )
pollers = utils.require_tree ( 'pollers' )
commands = utils.require_tree ( 'commands' )
actions = utils.require_tree ( 'actions' )

setmetatable ( commands, {
    __index = function ( self, index )
        return self.invalid
    end
} )

pollers.accept.clients:subscribe ( function ( client )
    pollers.clients:add ( client )
end )

pollers.accept.clients:subscribe ( function ( client )
    client.receive_message = utils.signal ()
    client.receive:subscribe ( function ( raw )
        local message = utils.build_message ( raw )
        message.from = client.id
        message.to = 'server'
        client.receive_message ( message )
    end )
    client.send_message = utils.signal ()
    client.send_message:subscribe ( function ( message )
        local message = utils.build_message ( message )
        message.from = 'server'
        message.to = client.id
        client.send ( message.raw )
    end )
end )

pollers.accept.clients:subscribe ( function ( client )
    client.receive_message:subscribe ( function ( message )
        print ( message.from .. ' <- ' .. message.raw )
    end )
    client.send_message:subscribe ( function ( message )
        print ( message.to .. ' -> ' .. message.raw )
    end )
end )

pollers.accept.clients:subscribe ( function ( client )
    client.receive_message:subscribe ( function ( message )
        commands [ message.type ] ( client, message )
    end )
end )

pollers.accept.clients:subscribe ( function ( client )
    client.send_message ( { type = 'welcome', client.id } )
end )

robots = {}

math.randomseed ( socket.gettime () )

timer = require 'timer' ( {
    function ()
        return pollers.clients:poll ()
    end,
    function ()
        return pollers.accept:poll ()
    end,
} )

timer:loop ()
