local SDL   = require 'SDL'
local image = require 'SDL.image'

local title  = "visualizer"
local width  = 800
local height = 600
local images = {}
local eventHandlers = {}
local windowPos = { x = 0, y = 0, scrollFactor = 1 }

local function errhandle ( ret, err )
    if not ret then
        error ( err )
    end
    return ret
end

local function initialize ()
    errhandle ( SDL.init { SDL.flags.Video } )
    errhandle ( image.init { image.flags.PNG } )

    window = errhandle ( SDL.createWindow {
        title  = title,
        width  = width,
        height = height,
        flags = { SDL.window.Resizeable },
    } )

    renderer = errhandle ( SDL.createRenderer ( window, -1 ) )
    renderer:setDrawColor ( 0x000000 )
end

local function loadTexture ( path )
    local surface = errhandle ( image.load ( path ) )
    local texture = errhandle ( renderer:createTextureFromSurface ( surface ) )
    return texture
end

local function flushEvents ()
    for event in SDL.pollEvent () do
        if eventHandlers [ event.type ] then
            eventHandlers [ event.type ] ( event )
        end
    end
end

local function renderPos ( pos )
    local rendered = {}
    rendered.w = pos.w * windowPos.scrollFactor
    rendered.h = pos.h * windowPos.scrollFactor

    rendered.x = pos.x + windowPos.x
    rendered.y = pos.y + windowPos.y

    local center = { x = width / 2, y = height / 2 }
    rendered.x = ( rendered.x - center.x ) * windowPos.scrollFactor + center.x
    rendered.y = ( rendered.y - center.y ) * windowPos.scrollFactor + center.y

    return rendered
end

local function draw ()
    renderer:clear ()
    for _, image in pairs ( images ) do
        renderer:copy ( image.texture, nil, renderPos ( image.pos ) )
    end
    renderer:present ()
end

return {
    initialize = initialize,
    draw = draw,
    delay = SDL.delay,
    flushEvents = flushEvents,
    eventHandlers = eventHandlers,
    loadTexture = loadTexture,
    images = images,
    windowPos = windowPos,
}
