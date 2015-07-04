# botstrats

#### A strategy game that makes you program robots

![screenshot][screenshot]

[screenshot]: http://i.imgur.com/oyEWW4m.png

## Idea

Unlike traditional strategy games, you do not control your units directly, but
instead write programs that control them using whatever programming language you
choose. They can start actions, like `move` or `scan`, to change and explore the
game state. The player only has direct control over one special unit – his
headquarter.

## Requirements

You’ll want to use the [Nix package manager][nix], which will install everything
you need as soon as you open up a `nix-shell` environment.

[nix]: http://nixos.org/nix/

## Quick try

Run each line in it’s own shell, and wait until the previos ones have settled:

```bash
nix-shell --run 'cabal run botstrats-server'
cd resources && nix-shell --run 'make'
nix-shell --run 'cabal run botstrats-visualizer'
nix-shell --run 'cabal run botstrats-sniffer'
nix-shell --run 'cabal run botstrats-supervisor'
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

## License

This project is licensed under [MIT (Expat)][license].

[license]: https://github.com/shak-mar/botstrats/blob/master/LICENSE
