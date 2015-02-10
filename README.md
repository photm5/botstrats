# botstrats

#### A strategy game that makes you program robots

![screenshot][1]

[1]: http://i.imgur.com/oyEWW4m.png

## idea

You program robots in whatever language you choose that can be used to create
executable files. The game starts these files and they communicate with it via
stdin and stdout. They can start actions, like `move` or `scan`, to change and
explore the game state. The player has direct control over a special building,
his headquarter. For example, he can open a TCP tunnel to a lua shell that runs
on the headquarter, and start actions that way.

## requirements

* lua
* luarocks:
    * `lua-cjson`
    * `lua-sdl2`
    * `luafilesystem`
* gnu bash
* gnu coreutils
* graphviz, if you want to view `doc/network.gv`
* inkscape, for rendering svg images
* gnu make
* netcat (Invoked as both `ncat` and `nc`. Whenever the `--sh-exec` option is
  needed, `ncat` is used.)
* rlwrap, If you want some commandline editing in your headquarter lua prompt.

This has only been tested on archlinux. If you successfully tried it on
something else, please open an issue so I can mention it here.

## quick try

You will need a lot of different terminals (or tmux windows/panes):

* `./server/server.sh`
* `./sniffer/sniffer.sh`
* `./visualizer/visualizer.sh`
* `./supervisor/supervisor.sh`
* `rlwrap nc localhost 2005`

In the lua prompt:
```lua
actions = io.open ( 'actions', 'a' )
results = io.open ( 'results', 'r' )

actions:write ( 'scan\n' ) actions:flush ()
repeat
    res = result:read ()
    print ( res )
until res == 'end_of_stream'
```

Controls in the visualizer:

| Key | Effect        |
| --- | ------------- |
| W   | Scroll up     |
| A   | Scroll left   |
| S   | Scroll down   |
| D   | Scroll right  |
| I   | Zoom in       |
| O   | Zoom out      |

You will want to zoom out until you see the headquarters building, and then
navigate to it. It is spawned at a random location. You cannot see the building
until you have started the `scan` action in the lua prompt.
