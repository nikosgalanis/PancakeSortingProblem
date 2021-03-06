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

### Running 

``` ghci pancakes.hs ```

### Examples

``` 
*Main> naive [2,7,3,1] 
[2,4,2,3,2] 

*Main> visualize [2,7,3,1] [2,4,2,3,2]
[[2,7,3,1],[7,2,3,1],[1,3,2,7],[3,1,2,7],[2,1,3,7],[1,2,3,7]]

*Main> bfs [12,23,9,36,20,34,4]
[2,3,6,2,4,3,2,7]

*Main> bidirectional [32,3,19,10,40,15,6,21,1]
[7,3,9,3,8,3,2,4,6]

*Main> batch [[6,12,1,7,3],[1,4,2,7,8],[10,2,11,8,9]]
[[4,3,5,3,4],[2,3,2],[2,3,5,3]]

*Main> gates  [12,23,9,36,20,34,4]
[4,5,2,3,7,3,7,5] 

*Main> burnt_bfs [(4,1),(9,0),(1,0),(10,1),(17,0)]
[1,4,2,4,3,4]

*Main> burnt_bidirectional [(4,1),(9,0),(1,0),(10,1),(17,0)]
[1,4,2,4,3,4]

*Main> burnt_naive [(4,1),(9,0),(1,0),(10,1),(17,0)]
[5,5,4,1,4,2,3,2,1,2]
```

### Contributors 

[Maria Despoina Siampou](https://github.com/msiampou) .

[Nikos Galanis](https://github.com/nikosgalanis) .


