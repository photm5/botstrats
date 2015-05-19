# botstrats concept

## network components

### Central Server (CS)

* Has control over the game state

### Supervisor (SV)

* Starts the robot programs
* Translates between robot and CS
* Is connected with CS over sockets

### Visualizer (VI)

* Gets information from SN or directly from CS

### Sniffer (SN)

* Sits between CS and SV
* Implemented using netcat, tee and named pipes
* Filters out informations to send to the VI
