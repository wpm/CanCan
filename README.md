KenKen Solver
=============

This package implements a solver and generator of [KenKen](http://www.kenken.com) puzzles.

The objective of KenKen puzzles is to completely fill an _n_ x _n_ grid with the numbers 1 to _n_.
Each row and column must contain a unique set of numbers.
(Thus a solved KenKen puzzle is a [Latin Square](http://en.wikipedia.org/wiki/Latin_square).)
Additionally, cells in the grid are grouped into sets called _cages_, and the sets of numbers in these cages must have certain arithmetic properties.

To see the program solve a puzzle, run either its unit tests or `Solver` with a path to a puzzle file.

	> scala -cp target/scala-2.9.2/classes kenken.Solver src/main/resources/6x6.1
	1.
    a=4x b=12x c=15+ d=3/ e=11+ f=1- g=11+ h=1- i=9+ j=2- k=60x l=3- m=4x n=1- o=5-
    a a b b c c
    a d d e e c
    f g h h i j
    f g g i i j
    k k l l m m
    k n n o o m

    1 2 3 4 5 6
    2 3 1 5 6 4
    5 6 2 1 4 3
    6 1 4 2 3 5
    4 5 6 3 2 1
    3 4 5 6 1 2

To generate a random set of puzzles, run `Generator` where the first argument is a number of puzzles and the second is the puzzle size.

	> scala -cp target/scala-2.9.2/classes kenken.Generator 2 5
	a=11+ b=10+ c=6x d=8+ e=1- f=60x g=6x h=40x i=5/
    a a b b c
    a d b c c
    e d f f f
    e d g g h
    i i g h h
    # 4 2 1 5 3
    # 5 3 4 2 1
    # 2 1 5 3 4
    # 3 4 2 1 5
    # 1 5 3 4 2

    a=10x b=10+ c=1- d=11+ e=6+ f=32x g=2 h=5 i=20x j=6x
    a a b b c
    a d b e c
    f d d e g
    f f h e i
    j j j i i
    # 1 2 4 5 3
    # 5 3 1 2 4
    # 4 5 3 1 2
    # 2 4 5 3 1
    # 3 1 2 4 5

The KenKen program is written in [Scala](http://www.scala-lang.org) and illustrates functional programming idioms.

Data Structures
---------------

The strategy is to use the constraints to eliminate possible values from the cells in the grid.

A _partial solution grid_ is an _n_ x _n_ grid of sets of numbers that are consistent with a puzzle's constraints.
In a completely unsolved grid, all the cells contain the numbers 1 to _n_.
A cell is _solved_ if it contains exactly one number.
A grid is solved if all its cells are solved.

A _constraint_ constrains the possible values that can appear in a grid.
There are two kinds of constraints: _cage constraints_ and _Latin Square_ constraints.
Cage constraints come in the following varieties:

* _Addition_

 The sum of the cells in a cage must equal a specified value.
* _Multiplication_

 The product of the cells in a cage must equal a specified value.
* _Subtraction_

 The difference of the cells in a cage must equal a specified value.
* _Division_

 The quotient of the cells in a cage must equal a specified value.
* _Specified_

 A single cell must contain a specified value.

Note that the cages for the non-associative operations must contain exactly two cells.
Either ordering of the cells will satisfy the constraint.

The Latin Square constraints require that numbers in rows and columns be unique.
For each row and column, this has two consequences which are analyzed as separate constraints:

* _Solved Cells_

 The values from all solved cells are removed from unsolved cells.
 For example, given the row {123} {1} {13}, this contraint returns {23} {1} {3}.
 The constraint is violated if all solved cells do not have distinct values.

* _Uniqueness_

 If a value only appears in a single cell in a row or column, it must be the solution for that cell.
 For example, given the column {23} {123} {23}, uniqueness returns {23} {1} {23}.

Given a partial solution grid, the constraints may serve to eliminate possible values from cells or reveal that partial solution as inconsistent with the puzzle.

Puzzle Solving
--------------

A brute force solution would simply try all possible solutions for a grid.
This can be implemented as a search of a directed graph of partial solution grids where the edges point from partial solutions to partial solutions containing where a candidate number has been removed from one of the cells.
Vertices with no outgoing edges are possible solutions.
Note that different sequences of guesses may lead to the same possible solution, so this search space is a directed acyclic graph.

A depth-first search of the graph starting from a completely unsolved grid will find all solutions.
Since there are _n_<sup>_n_<sup>2</sup></sup> possible solutions, a completely exhaustive search is infeasible.
However, at each guessed solution vertex we can apply all the constraints, then all the constraints that apply to any modified cells, and so on recursively.
This process is called _constraint propagation_, and eliminates possible values from some grids while revealing others as inconsistent with the constraints.
At each node it reduces the size of the search space to the point where an exhaustive search of the constrained graph is tractable.
Each search iteration makes a guess about the _least ambiguous cell_, i.e. the one with the fewest candidate numbers.

In this program, a partial solution is represented by the `Grid` object, which is a map of cell coordinates to sets of possible values.
The constraints are implemented as a hierarchy of `Constraint` objects which associate sets of cells coordinates with functions from possible values of those cells to constrained possible values.
The `Puzzle` object represents a puzzle.
It associates a set of cage constraints with a grid of a particular size.
A puzzle is passed to a `Solver` algorithm which finds to solutions.
The core algorithm is is implemented in the private recursive `search` function, which in turn calls a private recursive `applyConstraints` function.
The `solutions` value of a solver is a stream of all the its puzzle's solutions.

Puzzle Generation
-----------------

Puzzles are generated in the following way:

1. A random Latin Square is generated.
2. Contiguous cells in the grid are randomly grouped into cells.
3. An operation is randomly assigned to each cell and the value is calculated.

To generate the cells in step (2), find the connected components of a random sub-graph of a graph where every cell is adjacent to the ones with which it shares an edge.
The sizes of the connected components range between one and four and no more than 10% of the cells will be specified constraints.
There is no guarantee that the puzzle will have a unique solution.

References
----------

KenKen is a cousin of [Sudoku](http://en.wikipedia.org/wiki/Sudoku), and the algorithm implemented here is based on Peter Norvig's [Sudoku solver](http://norvig.com/sudoku.html).

Michael Heyeck has written a KenKen solver in Python called [neknek](http://www.mlsite.net/neknek).
This package can write puzzles in the format recognized by neknek.

"The complexity of completing partial Latin squares" Charles Colbourn, Discrete Applied Mathematics, Volume 8, Issue 1, April 1984, Pages 25-30
