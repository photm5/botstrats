# Robots game concept

## Design

### Rules

* Keep it simple
* Do it the Plan 9 way
* Don't restrict

### Ideas

* Use netcat instead of socket libraries
* Use shell programming languages

## network components

### Central Server (CS)

* Has control over the game state

### Supervisor (SV)

* Starts the robot programs
* Translates between robot and CS
* Is connected with CS over sockets

### User Interface (UI)

* Gets information from SN or directly from CS

### Sniffer (SN)

* Sits between CS and SV
* Implemented using netcat, tee and named pipes
* Filters out informations to send to the UI