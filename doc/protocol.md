# Protocol definition

## Central Server <-> Supervisor

### Basic structure

```
<uuid> <type> [<information>...]
```

### Types

#### Supervisor to Server

* `action <type> <robotuuid> [<information>...]`: A robot starts an action
* `spawn <type> <robotuuid> [<information>...]`: Spawn a robot under my control

#### Server to Supervisor

* `result <type> <robotuuid> [<information>...]`: The result of an action
* `access <target> <from> <target_supervisor>`: Special result of acces action,
  mount remote robot drive or determine local location and send that path to the
  robot
* `open_ftp <robotuuid>`: Start an ftp server serving the drive of that robot
* `spawn <type> <robotuuid>`: Spawn a robot under your control
* `start <robotuuid>`: Start a robot under your control

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
