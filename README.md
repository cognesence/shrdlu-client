# SHRDLU Client

The client side of the SHRDLU example used at EuroClojure 2017. You can see [a video of the talk here](https://www.youtube.com/watch?v=jzLSnadyYso).

## What is SHRDLU?

SHRDLU is a NetLogo model that simulates a robotic arm in a block world environment. It demonstrates basic principles of artificial intelligence and robotics within a constrained environment. The model provides a platform for executing a variety of commands to manipulate blocks of different shapes and colors, showcasing the capabilities of the robotic arm to interact with its surroundings.

## Features

After using our [Socket extension for NetLogo](https://github.com/cognesence/sock2) to connect this client to [the server-side software](https://github.com/cognesence/shrdlu-server) you'll be able to access:

- A simulated robotic arm (the "winch") with the ability to pick up, move, and drop blocks on command.
- Commands for direct manipulation of blocks within the environment.
- Customizable parameters for the environment and the winch.

## How to Use

1. **Setup:** Use our [Socket extension for NetLogo](https://github.com/cognesence/sock2) to connect this client to [the server-side software](https://github.com/cognesence/shrdlu-server). Then, initialize the client and robotic arm with the `setup` button.
2. **Commands:** Use SHRDLU commands to control the robotic arm and manipulate blocks.
3. **Experiment:** Modify the environment/parameters to observe different behaviors of the robotic arm.

## Installation

1. Ensure you have NetLogo installed on your system.
2. Download the `shrdlu.nlogo` file from the repository.
3. Open the file with NetLogo to start the simulation.

## Contributing

Contributions are welcome! Please fork the repository and submit pull requests with any enhancements or bug fixes.

## License

Copyright © 2017 Simon Lynch

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
