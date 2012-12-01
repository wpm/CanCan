package kenken

import annotation.tailrec
import scala.util.Random._

/**
 * Generator of random KenKen puzzles
 */
object Generator {
  /**
   * Probability of assigning an associative operator to a 2-cell cage
   */
  private val beta = 1 / 3.0
  /**
   * Percentage of cells that may be specified constraints
   */
  private val gamma = 0.05
  /**
   * Default distribution of cage sizes
   */
  private val defaultCageSizeDistribution = Multinomial(0, 0.07, 0.47, 0.46)
  /**
   * Maximum number of partial solutions before abandoning a generated grid
   */
  private val maxSearch = 1000

  /**
   * Generate a random puzzle and its unique solution.
   *
   * Ensure uniqueness by finding all solutions of the generated puzzle. If there is more than one, randomly change the
   * cage layout for all the cells that are not in cages that have the same values in all the solutions. Keep doing
   * this until we have a puzzle with a unique solution.
   *
   * @param n puzzle size
   * @param cageSize distribution from which to sample cage sizes
   * @return (solution, puzzle) tuple
   */
  def uniqueRandomPuzzle(n: Int, cageSize: Multinomial = defaultCageSizeDistribution): (Puzzle, Seq[Seq[Int]]) = {
    @tailrec
    def makeUnique(puzzle: Puzzle, solution: Seq[Seq[Int]], hint: Option[Grid] = None): Option[Puzzle] = {
      Solver.cappedSolutions(puzzle, maxSearch, hint) match {
        case (_, false) => None // Unable to find this puzzle's solutions: abandon it.
        case (grids, _) if (grids.size == 1) => Some(puzzle) // This puzzle has a unique solution.
        case (grids, _) => {
          // Partition cages into those whose values are equal across all the solutions and those who are not.
          val (equal, unequal) = puzzle.cageConstraints.partition {
            _.cells.forall(cell => grids.tail.forall(grid => grid(cell) == grids.head(cell)))
          }
          // Generate a new layout for the cells in the unequal cages. Skew the distribution of cage sizes towards the
          // largest allowed size.
          val unequalCells = unequal.flatMap(_.cells).toSeq
          val cages = randomCageLayout(unequalCells, Multinomial((1.0 :: List.fill(cageSize.max)(0.0)).reverse: _*))
          val constraints = cages.map(cage => randomCageConstraint(solution, cage.toSeq))
          // Use the values in the equal cells as the solution hint.
          val equalCells = equal.flatMap(_.cells).toSeq
          val hint = Grid(n) ++ equalCells.map(cell => (cell -> Set(solution(cell.row - 1)(cell.col - 1))))
          // Create a puzzle with the equal cages and the new cages.
          //          println("Cages " + unequal + ", Cells " + unequalCells.size + "\n" + hint + "\n")
          makeUnique(Puzzle(puzzle.n, equal ++ constraints), solution, Some(hint))
        }
      }
    }
    require(n > 1, "Invalid puzzle size " + n)
    val (puzzle, solution) = Iterator.continually {
      val (puzzle, solution) = randomPuzzle(n, cageSize)
      (makeUnique(puzzle, solution), solution)
    }.find {
      // Generate another puzzle if...
      _ match {
        case (None, _) => false // ...we can't find all the solutions for this one.
        case (Some(p), s) if (specifiedCells(n, p.cageConstraints.map(_.cells))) => true
        case _ => false // ...this one has too many specified constraints.
      }
    }.get
    (puzzle.get, solution)
  }

  /**
   * Generate a random KenKen puzzle and its solution.
   *
   * The puzzle is not guaranteed to have a unique solution.
   * @param n puzzle size
   * @param cageSize distribution from which to sample cage sizes
   * @return (solution, puzzle) tuple
   */
  def randomPuzzle(n: Int, cageSize: Multinomial): (Puzzle, Seq[Seq[Int]]) = {
    def puzzleFromLayout(solution: Seq[Seq[Int]], cages: Set[Set[Cell]]) =
      Puzzle(n, cages.map(cage => randomCageConstraint(solution, cage.toSeq)))

    require(n > 1, "Invalid puzzle size " + n)
    // Generate a random Latin Square then keep generating cage layouts for it until we have one that meets our
    // criteria.
    val solution = randomLatinSquare(n)
    val cells = for (x <- (1 to n); y <- (1 to n)) yield Cell(x, y)
    val cages = Iterator.continually(randomCageLayout(cells, cageSize)).find(specifiedCells(n, _)).get
    (puzzleFromLayout(solution, cages), solution)
  }

  // Does the puzzle have less than the maximum number of specified cells?
  private def specifiedCells(n: Int, cages: Set[_ <: Traversable[Cell]]): Boolean =
    cages.filter(cage => cage.size == 1).size <= scala.math.ceil(n * n * gamma)

  /**
   * Generate a random cage constraint from a cage and a solution grid
   */
  private def randomCageConstraint(solution: Seq[Seq[Int]], cage: Seq[Cell]): CageConstraint = {
    def randomAssociativeConstraint(values: Seq[Int]) = {
      nextInt(2) match {
        case 0 => PlusConstraint(values.sum, cage)
        case _ => TimesConstraint(values.product, cage)
      }
    }

    def randomNonAssociativeConstraint(values: Seq[Int]) = {
      require(values.size == 2)
      val (smaller, larger) = (values.sorted.head, values.sorted.last)
      require(smaller != larger)
      shuffle(MinusConstraint(larger - smaller, cage.head, cage.last) ::
        (if (larger % smaller == 0) List(DivideConstraint(larger / smaller, cage.head, cage.last)) else Nil)).head
    }

    val values = cage.map {
      case Cell(r, c) => solution(r - 1)(c - 1)
    }
    cage.size match {
      case 1 => SpecifiedConstraint(values.head, cage.head)
      case 2 => if (nextDouble <= beta) randomAssociativeConstraint(values) else randomNonAssociativeConstraint(values)
      case _ => randomAssociativeConstraint(values)
    }
  }

  /**
   * Generate a random Latin Square.
   *
   * Write a random permutation in the first row, generate successive rows by rotation, then shuffle the rows.
   * @param n the size of the Latin Square
   * @return a random Latin Square
   */
  def randomLatinSquare(n: Int): Seq[Seq[Int]] = {
    def rotate[E](xs: List[E]) = (xs.head :: xs.tail.reverse).reverse
    shuffle((List(shuffle((1 to n).toList)) /: (1 to n - 1)) {
      case (rows, _) => rotate(rows.head) :: rows
    })
  }

  /**
   * Randomly group adjacent cells in a grid into cages
   * @param cells cells in the grid
   * @param cageSize distribution from which to sample cage sizes
   * @return sets of cells in cages
   */
  private def randomCageLayout(cells: Seq[Cell], cageSize: Multinomial): Set[Set[Cell]] = {
    // Create a random graph between adjacent cells in a grid
    val edges = {
      /**
       * Given a cell, choose a random set of adjacent cells and create symmetric edges between them.
       */
      def randomUndirectedEdges(cell: Cell) = {
        // Cells above, below, to the left and right of a cell
        val adjacentCells = {
          for (x <- (-1 to 1);
               y <- (-1 to 1)
               if ((x == 0 || y == 0) && x != y)
          )
          yield Cell(x + cell.row, y + cell.col)
        }.filter(cells.contains(_))
        val randomAdjacent = shuffle(adjacentCells).take(nextInt(adjacentCells.size))
        randomAdjacent.flatMap(adjacent => Set(cell -> adjacent, adjacent -> cell))
      }

      // Draw edges between randomly adjacent cells and collect them into an adjacency list.
      val emptyAdjacency = Map[Cell, Set[Cell]]() ++ cells.map(cell => cell -> Set[Cell]())
      val edges = cells.flatMap(cell => randomUndirectedEdges(cell))
      (emptyAdjacency /: edges) {
        case (m, (s, d)) => m + (s -> (m(s) + d))
      }
    }

    connectedComponents(cells, edges(_: Cell), cageSize.sample())
  }

  /**
   * Find the connected components in an undirected graph
   *
   * This function requires that the graph be undirected, so the relationship
   * between nodes defined by `adjacent` must be symmetric.
   * @param ns the nodes in the graph
   * @param adjacent a function from a node to all its adjacent nodes
   * @param m maximum component size
   * @tparam N node type
   * @return list of sets of nodes in connected components
   */
  def connectedComponents[N](ns: Traversable[N], adjacent: N => Traversable[N], m: Int): Set[Set[N]] = {
    /**
     * Build a connected component from a set of frontier nodes.
     *
     * The size of the component is limited to a maximum value.
     * @param fs frontier nodes
     * @param vs visited nodes
     * @param cs current connected component
     * @return (visited nodes, connected component) tuple
     */
    @tailrec
    def connectedComponent(fs: List[N], vs: Set[N], cs: Set[N]): (Set[N], Set[N]) = {
      if (fs.isEmpty || cs.size >= m) (vs, cs)
      else {
        // The as are the nodes adjacent to the frontier not in the frontier and not previously visited.
        // They are added to the connected component. If necessary, this set is truncated so that the
        // connected component stays beneath its maximum size.
        val as = fs.flatMap(adjacent(_)).filter(n => !vs.contains(n) && !fs.contains(n)).take(m - cs.size)
        connectedComponent(as, vs ++ as ++ fs, cs ++ as)
      }
    }

    // The fold memo is (visited nodes, Set(connected component)).
    ((Set[N](), Set[Set[N]]()) /: ns) {
      case ((vs, css), n) => if (!vs.contains(n)) {
        val (nvs, ncs) = connectedComponent(List(n), vs, Set[N](n))
        (vs ++ nvs, css + ncs)
      } else (vs, css)
    }._2
  }

  /**
   * Discrete multinomial probability distribution over the support of the integers greater than zero.
   */
  case class Multinomial(ps: Double*) {
    private val cdf = (1 to ps.size).map(ps.map(_ / ps.sum).slice(0, _).sum)

    def sample() = cdf.indexWhere(_ > nextDouble) + 1

    def max: Int = cdf.size

    override def toString = cdf.map("%.3f".format(_)).zipWithIndex.map(t => t._2 + ":" + t._1).mkString(" ")
  }

  /**
   * Cage size distribution in puzzles.
   * @param puzzles set of puzzles
   * @return table of cage size distribution across the puzzles
   */
  def empiricalCageSizeDistribution(puzzles: Traversable[Puzzle]) = {
    val f = (Map[Int, Int]() /: puzzles.flatMap(_.cageConstraints).
      map(_.cells.size))((m, v) => m + (v -> (m.getOrElse(v, 0) + 1)))
    val d = f.values.sum.toDouble
    f.toList.map(t => (t._1, t._2 / d)).sorted.map(t => "%d:%.3f".format(t._1, t._2)).mkString("\n")
  }

  private val usage =
    """generate [-n] puzzles size
      |
      |Generate puzzles. By default generate puzzles with unique solutions.
      |
      |    puzzles - the number of puzzles to generate
      |    size - the size of the puzzle
      |
      |    -n - generate puzzles that may have non-unique solutions
      |
      |At the end this prints the distribution of cage sizes in the puzzles.""".stripMargin

  def main(args: Array[String]) {
    def parseCommandLine(args: Array[String]): (Int, Int, Boolean) = {
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Map[Symbol, String]): (List[String], Map[Symbol, String]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-n" :: tail => parseCommandLineRec(tail, positional, option + ('nonUnique -> ""))
          case "-h" :: tail => CanCan.error(usage)
          case s :: tail if (s(0) == '-') => CanCan.error("Invalid switch " + s)
          case arg :: tail if (arg.matches( """\d+""")) => parseCommandLineRec(tail, arg :: positional, option)
          case arg :: tail => CanCan.error("Invalid argument " + arg)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      CanCan.errorIf(positional.size != 2, "Invalid number of arguments")
      (positional(0).toInt, positional(1).toInt, !option.contains('nonUnique))
    }

    def prepend(s: String, prefix: String) = s.split("\n").map(prefix + _).mkString("\n")

    val (numPuzzles, n, unique) = parseCommandLine(args)
    val cageSize = defaultCageSizeDistribution

    var puzzles = for (_ <- (1 to numPuzzles).toStream)
    yield if (unique) uniqueRandomPuzzle(n, cageSize) else randomPuzzle(n, cageSize)
    for (((puzzle, solution), i) <- puzzles.zipWithIndex)
      println("# " + (i + 1) + ".\n" + puzzle + "\n" +
        prepend(StringRepresentation.tableToString(solution), "# ") + "\n")
    println("\n" + prepend(empiricalCageSizeDistribution(puzzles.map(_._1)), "# "))
  }
}
