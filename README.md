CanCan - a KenKen Solver and Generator
======================================

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
    a=9+ b=48x c=5 d=8x e=10x f=8+ g=8+ h=8x i=6x j=2-
    a b b c d
    a b e e d
    a f f g d
    h f i g j
    h h i g j
    # 1 3 4 5 2
    # 3 4 5 2 1
    # 5 2 1 3 4
    # 4 5 2 1 3
    # 2 1 3 4 5
    # Difficulty: 1

    # 2.
    a=10+ b=4- c=8+ d=8+ e=24x f=11+ g=5+ h=10x i=3 j=3-
    a b b c c
    a d d c e
    a d f e e
    g f f h h
    g i j j h
    # 2 5 1 4 3
    # 3 2 5 1 4
    # 5 1 4 3 2
    # 1 4 3 2 5
    # 4 3 2 5 1
    # Difficulty: 1

    # Cage size, Single cell cage proportion, Associative probability
    # [0:0.000, 1:0.050, 2:0.350, 3:0.350, 4:0.200, 5:0.050], 0.200, 0.333
    # Unique solutions, maximum search 1000
    # Average difficulty: 1.000
    # Cage Size Macro Average:
    # 1: 0.100
    # 2: 0.300
    # 3: 0.600

The `analyze` command inspects the details of the search procedure used to solve a set of puzzles.

In the build directory the `sbt run` command can also be used to run the program.

References
----------

KenKen is a cousin of [Sudoku](http://en.wikipedia.org/wiki/Sudoku), and the algorithm implemented here is based on Peter Norvig's [Sudoku solver](http://norvig.com/sudoku.html).

A.M. Herzberg and M.R. Murty, "Sudoku squares and chromatic polynomials", *Notices of the American Mathematical Society*, June/July 2007, pp. 708-717

Michael Heyeck has written a KenKen solver in Python called [neknek](http://www.mlsite.net/neknek).
CanCan can write puzzles in the format recognized by neknek.

Charles Colbourn, "The complexity of completing partial Latin squares", *Discrete Applied Mathematics*, April 1984, Volume 8, Issue 1, pp. 25-30

J.F. Cook, "A pencil-and-paper algorithm for solving Sudoku puzzles", *Notices of the American Mathematical Society*, April 2009, Volume 56, Number 4, pp. 460-468
