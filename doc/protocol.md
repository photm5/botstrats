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
* `robot_stopped <robotuuid>`: A robot under my control has stopped running

#### Server to Supervisor

* `result <type> <robotuuid> [<information>...]`: The result of an action
* `access <target> <from> <target_supervisor>`: Special result of acces action,
  mount remote robot drive or determine local location and send that path to the
  robot
* `open_ftp <robotuuid>`: Start an ftp server serving the drive of that robot
* `spawn <type> <robotuuid>`: Spawn a robot under your control
* `start <robotuuid>`: Start a robot under your control

## Robot -> Supervisor

### Basic structure

```
<actiontype> [<information>...]
```

### Types

* `move <north/east/south/west>`: engineer, specialist  
    Move in that direction
* `access <uuid>`: specialist, factory, headquarters  
    Return a path under which the drive of that robot can be accessed
* `start <uuid>`: specialist, factory, headquarters  
    Start that robot under the control of my supervisor
* `spawn <type> [<x> <y>]`: engineer, factory, headquarters  
    Spawn a robot of that type at that position and return its uuid
* `query`: *everyone*  
    Return json-formatted information about me. Example:
```
success {"type":"headquarters","uuid":"8800a95b-12b3-454a-b527-7df0fc07501f","x":"-27","y":"4"}
```
* `scan`: *everyone*  
    Return json-formatted information about the robots surrounding me as a
    stream. Example:
```
success begin_of_stream
{"type":"headquarters","uuid":"8800a95b-12b3-454a-b527-7df0fc07501f","x":"-27","y":"4","status":"scan"}
{"type":"engineer","uuid":"876cf8d3-01c3-4441-942c-bc9bf3926fb0","x":"-26","y":"4","status":"off"}
end_of_stream
```

## Supervisor -> Robot

### Basic structure

* on a single line:

```
<success> [<information>...]
```

* or as a stream:

```
<success> begin_of_stream
[<information split by newlines>...]
end_of_stream
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
