#!/bin/env lua

graphics = require 'graphics'
SDL = require 'SDL'
lfs = require 'lfs'

args = { ... }

queue_folder = args [ 1 ]

local running = true
graphics.eventHandlers [ SDL.event.Quit ] = function ( e )
    running = false
end

graphics.initialize ()

local keyboardState = {}

graphics.eventHandlers [ SDL.event.KeyDown ] = function ( e )
    keyboardState [ e.keysym.sym ] = true
end
graphics.eventHandlers [ SDL.event.KeyUp ] = function ( e )
    keyboardState [ e.keysym.sym ] = false
end

function checkKeys ()
    local scrollBy = 0.02
    local moveBy = 10
    local scrollFactor = graphics.windowPos.scrollFactor
    if keyboardState [ SDL.key.i ] then
        graphics.windowPos.scrollFactor = scrollFactor + scrollFactor * scrollBy
    end
    if keyboardState [ SDL.key.o ] then
        graphics.windowPos.scrollFactor = scrollFactor - scrollFactor * scrollBy
    end
    if keyboardState [ SDL.key.w ] then
        graphics.windowPos.y = graphics.windowPos.y + ( 1 / scrollFactor ) * moveBy
    end
    if keyboardState [ SDL.key.a ] then
        graphics.windowPos.x = graphics.windowPos.x + ( 1 / scrollFactor ) * moveBy
    end
    if keyboardState [ SDL.key.s ] then
        graphics.windowPos.y = graphics.windowPos.y - ( 1 / scrollFactor ) * moveBy
    end
    if keyboardState [ SDL.key.d ] then
        graphics.windowPos.x = graphics.windowPos.x - ( 1 / scrollFactor ) * moveBy
    end
end

function splitstr ( s )
    split = {}
    for s in string.gmatch ( s, '[^ ]+' ) do
        table.insert ( split, s )
    end
    return split
end

function handleDataLine ( line )
    print ( line )
    split = splitstr ( line )
    if #split < 6 then return end
    uuid = split [ 1 ]
    path = split [ 2 ]
    w = tonumber ( split [ 3 ] )
    h = tonumber ( split [ 4 ] )
    x = tonumber ( split [ 5 ] )
    y = tonumber ( split [ 6 ] )
    time = tonumber ( split [ 7 ] ) or -1
    graphics.images [ uuid ] = {
        texture = graphics.loadTexture ( path ),
        pos = { w = w, h = h, x = x, y = y },
        alphaMod = 255,
        stayTime = time
    }
end

function checkQueue ()
    for file in lfs.dir ( queue_folder ) do
        file = queue_folder .. '/' .. file
        if lfs.attributes ( file, 'mode' ) == 'file' then
            f = io.open ( file )
            for line in f:lines () do
                if line then
                    handleDataLine ( line )
                end
            end
            os.execute ( 'rm ' .. file )
        end
    end
end

i = 0
while running do
    checkQueue ()
    checkKeys ()
    graphics.flushEvents ()
    graphics.draw ()
    graphics.delay ( 4 )
    for _, image in pairs ( graphics.images ) do
        if image.stayTime ~= -1 then
            image.alphaMod = image.alphaMod - 255 / image.stayTime
            if image.alphaMod < 0 then
                image.alphaMod = 0
            end
        end
        image.texture:setAlphaMod ( image.alphaMod )
    end
end
