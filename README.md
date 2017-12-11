# typelevel-dijkstra-sp
Dijkstra's shortest path algorithm implemented at type level with Haskell

## How To Run
```bash
$ ghci Dijkstra.hs
> :kind! Solution
= '[0, 6, 2, 5]
```

## How to play with the graph
The type synonym `Adj` contains the graph data in adjacency matrix format. Here is the convention we follow:

* Every node has 0 distance to itself
* Every node has infinite (`Inf`) distance to any of its non-neighbour

There is an example test case, which is named `test_result_1`. If you change the constraint array on there, the module will not compile.
