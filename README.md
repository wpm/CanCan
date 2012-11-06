KenKen Solver
=============

This solves [KenKen](http://www.kenken.com) puzzles.

Based on Peter Norvig's [Sudoku solver](http://norvig.com/sudoku.html).

Represent partial solutions as a grid of possible values.

Grid of numbers with constraints.

Brute force search is too large.

Do a depth-first search of the solution graph.

(The graph is a DAG.)

At each node in the graph, apply the constraints.

When a constraint is applied, it can change the contents of the
grid, which can cause other constraints to fire.
Propagate all constraint changes recursively.

The search algorithm finds all possible solutions. But there may
be multiple paths through the graph to a solution, so maintain a
set of solutions that have been seen.

If you just want to find the first solution, this list will always be constant size.
