# botstrats

#### A strategy game that makes you program robots

![screenshot][1]

[1]: http://i.imgur.com/oyEWW4m.png

## idea

You program robots in whatever language you choose that can be used to create
executable files. The game runs these files and communicates with them via
stdin and stdout. They can start actions, like `move` or `scan`, to change and
explore the game state. The player has direct control over a special building,
his headquarter, by opening a TCP tunnel to a netcat.

## requirements

To be updated as soon as the lua dependency is removed.

## quick try

* server:

    cd server
    nix-shell
    cabal configure
    cabal run

* `./supervisor/supervisor.sh`
* `rlwrap nc localhost 2005` In here, you can run commands from the point of
  view of your headquarter. Try `query`, `scan`, `spawn engineer`, etc.

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
until you have started the `scan` action in the prompt.

## license

This project is licensed under [MIT (Expat)][license].

[license]: https://github.com/shak-mar/botstrats/blob/master/LICENSE
