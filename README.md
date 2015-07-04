# botstrats

#### A strategy game that makes you program robots

![screenshot][1]

[1]: http://i.imgur.com/oyEWW4m.png

## idea

Unlike traditional strategy games, you do not control your units directly, but
instead write programs that control them using whatever programming language you
choose. They can start actions, like `move` or `scan`, to change and explore the
game state. The player only has direct control over one special unit – his
headquarter.

## requirements

All the requirements should be recorded in the `shell.nix` files.

## quick try

Run each line in it’s own shell, and wait until the previos ones have settled:

```
cd server && nix-shell --run 'cabal run'
cd visualizer && nix-shell --run 'cabal run'
cd sniffer && nix-shell --run 'cabal run'
cd supervisor && nix-shell --run 'cabal run'
nix-shell -p rlwrap --run 'rlwrap nc localhost 2005'
```

In the last shell, you can then run commands from the POV of your headquarter.
Try `query`, `scan`, `spawn engineer`.

You can use the mouse to control the visualizer:

* Zoom by utilizing the scroll wheel
* Move the view port by left-clicking and dragging
* Rotate the view port by right-clicking and dragging

You will want to zoom out until you see the headquarters building, and then
navigate to it. It is spawned at a random location. You cannot see the building
until you have started the `scan` action in the prompt.

## license

This project is licensed under [MIT (Expat)][license].

[license]: https://github.com/shak-mar/botstrats/blob/master/LICENSE
