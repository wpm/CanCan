CanCan - a KenKen Solver
========================

CanCan is a solver and generator of [KenKen](http://www.kenken.com) puzzles.

The objective of KenKen puzzles is to completely fill an _n_ x _n_ grid with the numbers 1 to _n_.
Each row and column must contain a unique set of numbers.
(Thus a solved KenKen puzzle is a [Latin Square](http://en.wikipedia.org/wiki/Latin_square).)
Additionally, cells in the grid are grouped into sets called _cages_, and the sets of numbers in these cages must have certain arithmetic properties.

CanCan is built with the [Simple Build Tool](http://www.scala-sbt.org).
The `sbt assembly` command builds an executable .JAR file.
Run the default executable without any arguments to see help.

The `solve` command solves a set of puzzles.

    > java -jar target/CanCan-assembly-1.0.jar solve puzzles/6x6.1
    1.
    a=60x b=4+ c=16+ d=45x e=40x f=72x g=4x h=11+ i=4- j=12+ k=12x l=5 m=8+ n=2
    a b b c c d
    a b e c d d
    a f e e g g
    f f h i j g
    k l h i j j
    k k h m m n

    5 2 1 4 6 3
    2 1 4 6 3 5
    6 3 5 2 1 4
    4 6 3 5 2 1
    3 5 2 1 4 6
    1 4 6 3 5 2

The `generate` command generates a set of puzzles of a specified size.

	> java -jar target/CanCan-assembly-1.0.jar generate 2 5
	# 1.
	a=10+ b=8+ c=60x d=8+ e=7+ f=2 g=20x h=24x i=3 j=5x
	a b b c c
	a b d c e
	a d d f e
	g g h h e
	g i h j j
	# 2 1 5 4 3
	# 3 2 1 5 4
	# 5 4 3 2 1
	# 1 5 4 3 2
	# 4 3 2 1 5

	# 2.
	a=8+ b=11+ c=24x d=25x e=6x f=2 g=9+ h=12x i=5x j=5
	a b b b c
	a a d d c
	e e d f c
	e g g h i
	j g h h i
	# 1 5 2 4 3
	# 4 3 1 5 2
	# 3 1 5 2 4
	# 2 4 3 1 5
	# 5 2 4 3 1


	# 1:0.200
	# 2:0.100
	# 3:0.700

The `analyze` command is used to analyze the difficulty of a set of puzzles.

In the build directory the `sbt run` command can also be used to run the program.

The KenKen program is written in [Scala](http://www.scala-lang.org) and illustrates functional programming idioms.

Puzzle Solving
--------------

A brute force solution would simply try all possible solutions for a grid.
This can be implemented as a search of a directed graph of partial solution grids where the edges point from partial solutions to partial solutions containing where a candidate number has been removed from one of the cells.
Vertices with no outgoing edges are possible solutions.

A depth-first search of the graph starting from a completely unsolved grid will find all solutions.
Since there are _n_<sup>_n_<sup>2</sup></sup> possible solutions, a completely exhaustive search is infeasible.
However, at each guessed solution vertex CanCan applies all the constraints, then all the constraints that apply to any modified cells, and so on recursively.
This process is called _constraint propagation_, and eliminates possible values from some grids while revealing others as inconsistent with the constraints.
At each node it reduces the size of the search space to the point where an exhaustive search of the constrained graph is tractable.
Each search iteration makes a guess about the _least ambiguous cell_, i.e. the one with the fewest candidate numbers.

Puzzle Generation
-----------------

Puzzles are generated in the following way:

1. A random Latin Square is generated.
2. Contiguous cells in the grid are randomly grouped into cells.
3. An operation is randomly assigned to each cell and the value is calculated.
4. If the puzzle has multiple solutions, it is modified until it has only one.

To generate the cells in step (2), find the connected components of a random sub-graph of a graph where every cell is adjacent to the ones with which it shares an edge.
The sizes of the connected components range between one and four and no more than 10% of the cells will be specified constraints.
In step (4) the cages that do not contain the same values across all solutions are randomly redrawn until a unique solution exists.

References
----------

KenKen is a cousin of [Sudoku](http://en.wikipedia.org/wiki/Sudoku), and the algorithm implemented here is based on Peter Norvig's [Sudoku solver](http://norvig.com/sudoku.html).

Michael Heyeck has written a KenKen solver in Python called [neknek](http://www.mlsite.net/neknek).
This package can write puzzles in the format recognized by neknek.

"The complexity of completing partial Latin squares" Charles Colbourn, Discrete Applied Mathematics, Volume 8, Issue 1, April 1984, Pages 25-30
