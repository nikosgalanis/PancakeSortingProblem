# PancakeSortingProblem

[Pancake sorting](https://en.wikipedia.org/wiki/Pancake_sorting) is a problem of sorting a disordered stack of pancakes in order of size when a spatula can be inserted at any point in the stack and used to flip all pancakes above it.

The chef in our place is sloppy, and when he prepares a stack of pancakes they come out all different sizes. Therefore, when I deliver them to a customer, on the way to the table, I rearrange them (so that the smallest winds up on top, and so on, down to the largest at the bottom), by grabbing several from the top and flipping them over, repeating this (varying the number I flip) as many times as necessary. If there are n pancakes, what is the maximum number of flips that I will every have to use to rearrange them?

We use some different algorithms to approach the problem.

A naive sort, with maximum 2n flips.

A BFS sort.

A bidirectional sort, which uses 2 different BFSs.

A batch sort, so given a big list of different lists, we use the minimum possible BFSs.

The algorithm implemented by Bill Gates and Christos Papadimitriou, with maximum (5n+5)/3 flips.
(https://people.eecs.berkeley.edu/~christos/papers/Bounds%20For%20Sorting%20By%20Prefix%20Reversal.pdf)

We also give the same solutions(except the last one), for burnt pancakes, so in the goal state, the burnt side is facing downwards.

### Getting Started
Install the Haskell toolchain : https://www.haskell.org/downloads

### Downloading
Download source code by typing:

``` git clone https://github.com/nikosgalanis/PancakeSortingProblem.git ```


