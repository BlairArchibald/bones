# Bones

Bones is a Haskell based skeleton library which builds on HdpH for doing
distributed memory branch-and-bound experimentation. We distribute three test
applications with the library:

- maxclique: Find the maximum clique in a (DIMACS formatted) graph
- knapsack: Find the optimal packing for a knapsack
- tsp: Find the shortest tour between cities

## Install

The easiest way to install is to use the provided stack.yaml file and the [stack
tool](https://docs.haskellstack.org/en/stable/README/). This manages downloading
the correct compiler (GHC 8.0.1), HdpH and additional dependencies.

## Running

Each test application comes with it's own help output. A common command will look like this following:

`stack exec bones-<appname> -- -a <skeletonType> -f <inputfile> -d <spawnDepth> +HdpH numProcs=1 scheds=3 -HdpH +RTS -N4 -RTS`

Generally you will want to use a job launcher to launch multiple processes. In
this case you can't use stack exec due to file locking. Instead use the raw
binary path:

`mpiexec -n 2 ./.stackwork/install/bin/<system>/lts-7.9/8.0.1/bones-<appname> -a <skeletonType> -f <inputfile> -d <spawnDepth> +HdpH numProcs=2 scheds=3 -HdpH +RTS -N4 -RTS`