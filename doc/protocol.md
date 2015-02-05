# Protocol definition

## Central Server <-> Supervisor

### Basic structure

```
<uuid> <type> [<information>...]
```

### Types

* `action <type> <robotuuid>`, SV to CS: A robot starts an action
* `spawn <type> <robotuuid>`, CS to SV: Spawn a new robot

## Supervisor <-> Robot

### Basic structure

```
<actiontype> [<information>...]
```

## User Interface

When the user interface receives a message of this form:

```
<id> <path> <width> <height> <x> <y> [<ticks>]
```

it will display the image found at `<path>` with width `<width>`, height
`<height>`, x position `<x>` and y position `<y>`. When another message is sent
with the same `<id>`, it will move the first image to the new position/size.

If `<ticks>` is given, the image will slowly fade until it is completely
transparent. `<ticks>` specifies how long it takes.
