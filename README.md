KenKen Solver
=============

KenKen is a solver for [KenKen](http://www.kenken.com) puzzles.
To see it solve a puzzle, run the unit tests or run `KenKen` with a path to a puzzle file.

	> scala -cp target/scala-2.9.2/classes kenken.KenKen src/main/resources/puzzle
	2 1 4 3
	4 3 2 1
	1 4 3 2
	3 2 1 4

The objective of KenKen puzzles is to completely fill an _n_x_n_ grid with the numbers 1 to _n_.
Each row and column must contain a unique set of numbers.
(Thus a solved KenKen puzzle is a [Latin Square](http://en.wikipedia.org/wiki/Latin_square).)
Additionally, cells in the grid are grouped into sets called _cages_, and the sets of numbers in these cages must have certain arithmetic properties.

The KenKen program is written in [Scala](http://www.scala-lang.org) and illustrates functional programming idioms.

Data Structures
---------------

The strategy is to use the constraints to eliminate possible values from the cells in the grid.

A _partial solution grid_ is an _n_x_n_ grid of sets of numbers that are consistent with a puzzle's constraints.
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

* _Definiteness_

 All solved cells in a row or column must have different values. For example, given the row {123} {1} {13}, definiteness returns {23} {1} {3}.

* _Uniqueness_

 If a value only appears in a single cell in a row or column, it must be the solution for that cell. For example, given the column {23} {123} {23}, uniqueness returns {23} {1} {23}.

Given a partial solution grid, the constraints may serve to eliminate possible values from cells or reveal that partial solution as inconsistent with the puzzle.

Algorithm
---------

A brute force solution would simply try all possible solutions for a grid.
This could be implemented as a search of a directed graph of partial solution grids where the edges point from partial solutions to partial solutions containing a guess for one of the cells.
Vertices with no outgoing edges are possible solutions.
Note that difference sequences of guesses may lead to the same possible solution, so the search space is a directed acyclic graph.

To search for solutions, do a depth-first search of the graph starting from a completely unsolved
grid.
Since there are _n_<sup>_n_<sup>2</sup></sup> possible solutions, a completely exhaustive search is infeasible.
However, at each guessed solution vertex we apply all the constraints, then all the constraints that apply to any modified cells, and so on recursively.
This process is called _constraint propagation_, and will eliminate possible values from some grids and reveal others as inconsistent with the constraints.
At each node it reduces the size of the search space to the point where an exhaustive search of the constrained graph is tractable.

Implementation
--------------

A partial solution is implemented in the `Grid` object, which is a map of cell coordinates to sets of possible values.
The constraints are implemented as a hierarchy of `Constraint` objects which associate sets of cells coordinates with functions from possible values of those cells to constrained possible values.
The `KenKen` object represents a single puzzle.
It associates a set of constraints with a grid of a particular size.
The search algorithm is implemented in the private recursive `search` function, which in turn calls a private recursive `propagateConstraints` function.

The `KenKen.solutions` method returns all solutions as a lazily-evaluated sequence.
Because the search algorithm may find the same solution via multiple paths, this method maintains a private set of all solved grids it has encountered so far.
If you just want to find a single solution, you may call the `KenKen.solution` method instead.

References
----------

KenKen is a cousin of [Sudoku](http://en.wikipedia.org/wiki/Sudoku), and the algorithm implemented here is based on Peter Norvig's [Sudoku solver](http://norvig.com/sudoku.html).

Michael Heyeck has written a KenKen solver in Python called [neknek](http://www.mlsite.net/neknek).
