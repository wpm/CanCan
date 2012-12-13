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

References
----------

KenKen is a cousin of [Sudoku](http://en.wikipedia.org/wiki/Sudoku), and the algorithm implemented here is based on Peter Norvig's [Sudoku solver](http://norvig.com/sudoku.html).

Michael Heyeck has written a KenKen solver in Python called [neknek](http://www.mlsite.net/neknek).
CanCan can write puzzles in the format recognized by neknek.

Charles Colbourn, "The complexity of completing partial Latin squares", *Discrete Applied Mathematics*, April 1984, Volume 8, Issue 1, pp. 25-30

J.F. Cook, "A Pencil-and-Paper Algorithm for Solving Sudoku Puzzles", *Notices of the American Mathematical Society*, April 2009, Volume 56, Number 4, pp. 460-468
