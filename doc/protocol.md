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
