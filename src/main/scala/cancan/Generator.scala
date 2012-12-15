package cancan

import annotation.tailrec
import scala.util.Random._
import util.parsing.combinator.JavaTokenParsers
import cancan.Generator.Multinomial.NumberListParser

/**
 * Generator of random KenKen puzzles
 *
 * Puzzles are generated in the following way:
 *
 * 1. A random Latin Square is generated.
 * 2. Contiguous cells in the grid are randomly grouped into cages.
 * 3. An operation is randomly assigned to each cell and the value is calculated.
 *
 * To generate the cages in step (2), find the connected components of a random sub-graph of a graph where every cell
 * is adjacent to the ones with which it shares an edge. The caller can specify a distribution from which cage sizes
 * are drawn and a maximum proportion of single-cell cages per puzzle. If necessary this procedure is repeated until
 * a unique solution is found.
 *
 * The difficulty of a generated puzzle is the number of steps it takes the [[cancan.OracleSolver]] to solve it.
 */
object Generator {
  /**
   * Maximum number of partial solutions before abandoning a generated grid
   */
  private val defaultMaxSearch = 1000
  /**
   * Default distribution of cage sizes
   */
  private val defaultCageSizeDistribution = Multinomial(0, 0.05, 0.35, 0.35, 0.2, 0.05)
  /**
   * Portion of cages that may consist of a single cell
   */
  private val defaultSingleCellProportion = 0.2
  /**
   * Probability of assigning an associative operator to a 2-cell cage
   */
  private val defaultAssociativeProbability = 1 / 3.0

  /**
   * Generate a random puzzle and its unique solution.
   *
   * Continually generate puzzles until we find one with a unique solution.
   *
   * @param n puzzle size
   * @param cageSize distribution from which to sample cage sizes
   * @param maxSearch maximum number of partial solutions to explore before trying another puzzle
   * @return (solution, puzzle) tuple
   */
  def uniqueRandomPuzzle(n: Int,
                         cageSize: Multinomial = defaultCageSizeDistribution,
                         maxSearch: Int = defaultMaxSearch,
                         specifiedProportion: Double = defaultSingleCellProportion,
                         associativeProbability: Double = defaultAssociativeProbability): (Puzzle, Seq[Seq[Int]]) = {
    val (puzzle, solution, _) = Iterator.continually {
      val (puzzle, solution) = randomPuzzle(n, cageSize, specifiedProportion, associativeProbability)
      val (ss, complete) = cappedSolutions(puzzle, maxSearch)
      (puzzle, solution, complete && ss.take(2).size == 1)
    }.find(_._3).get
    (puzzle, solution)
  }

  /**
   * Generate a random KenKen puzzle and its solution.
   *
   * The puzzle is not guaranteed to have a unique solution.
   * @param n puzzle size
   * @param cageSize distribution from which to sample cage sizes
   * @return (solution, puzzle) tuple
   */
  def randomPuzzle(n: Int,
                   cageSize: Multinomial = defaultCageSizeDistribution,
                   specifiedProportion: Double = defaultSingleCellProportion,
                   associativeProbability: Double = defaultAssociativeProbability): (Puzzle, Seq[Seq[Int]]) = {
    def puzzleFromLayout(solution: Seq[Seq[Int]], cages: Set[Set[Cell]]) =
    // Note that cage.toSeq returns an ArrayBuffer, which is a mutable object.
      Puzzle(n, cages.map(cage => randomCageConstraint(solution, cage.toList, associativeProbability)))

    require(n > 1, "Invalid puzzle size " + n)
    // Generate a random Latin Square then keep generating cage layouts for it until we have one that meets our
    // criteria.
    val solution = randomLatinSquare(n)
    val cells = for (x <- (1 to n); y <- (1 to n)) yield Cell(x, y)
    val cages =
      Iterator.continually(randomCageLayout(cells, cageSize)).find(specifiedCells(_, specifiedProportion)).get
    (puzzleFromLayout(solution, cages), solution)
  }

  // Does the puzzle have an acceptable number of single-cell constraints?
  private def specifiedCells(cages: Set[_ <: Traversable[Cell]], specifiedProportion: Double): Boolean = {
    val size1Cages = cages.filter(cage => cage.size == 1).size
    size1Cages / cages.size.toDouble <= specifiedProportion
  }

  /**
   * Generate a random cage constraint from a cage and a solution grid
   */
  private def randomCageConstraint(solution: Seq[Seq[Int]],
                                   cage: Seq[Cell],
                                   associativeProbability: Double): CageConstraint = {
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
      case 2 => if (nextDouble <= associativeProbability)
        randomAssociativeConstraint(values)
      else randomNonAssociativeConstraint(values)
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
    private val psNorm = ps.map(_ / ps.sum)
    private val cdf = (1 to ps.size).map(psNorm.slice(0, _).sum)

    def sample() = cdf.indexWhere(_ > nextDouble) + 1

    def max: Int = cdf.size

    override def toString =
      psNorm.map("%.3f".format(_)).zipWithIndex.map(t => t._2 + ":" + t._1).mkString("[", ", ", "]")
  }

  object Multinomial {
    def apply(s: String): Multinomial = Multinomial(s.split(",").map(_.toDouble): _*)

    object NumberListParser extends JavaTokenParsers {
      def number: Parser[Double] = decimalNumber ^^ (_.toDouble)

      def numbers: Parser[List[Double]] = rep1sep(number, ",")

      def matches(s: String): Boolean = parseAll(numbers, s).successful
    }

  }

  /**
   * Generate a set of puzzles.
   */
  def generate(args: Array[String]) {
    val usage =
      """generate [-n|-m|-c|-s|-a] puzzles size
        |
        |Generate puzzles. By default generate puzzles with unique solutions.
        |
        |    puzzles - the number of puzzles to generate
        |    size - the size of the puzzle
        |
        |    -n - generate puzzles that may have non-unique solutions
        |    -m - maximum partial solutions to search when looking for unique solutions (default %d)
        |    -c ##[,##...] - cage size distribution default %s
        |    -s ## - maximum proportion of cages that may be a single cell (default %.2f)
        |    -a ## - probability a 2-cell cage with be associative (default %.2f)
        |
        |At the end this prints the distribution of cage sizes in the puzzles.""".stripMargin.format(
        defaultMaxSearch,
        defaultCageSizeDistribution,
        defaultSingleCellProportion,
        defaultAssociativeProbability)

    def parseCommandLine(args: Array[String]): (Int, Int, Boolean, Int, Multinomial, Double, Double) = {
      @tailrec
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Map[Symbol, String]): (List[String], Map[Symbol, String]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-n" :: tail => parseCommandLineRec(tail, positional, option + ('nonUnique -> ""))
          case "-m" :: m :: tail if (m.matches( """\d+""")) =>
            parseCommandLineRec(tail, positional, option + ('maxSearch -> m))
          case "-c" :: m :: tail if (NumberListParser.matches(m)) =>
            parseCommandLineRec(tail, positional, option + ('cageDistribution -> m))
          case "-s" :: m :: tail if (m.matches( """0(\.\d*)?""")) =>
            parseCommandLineRec(tail, positional, option + ('specified -> m))
          case "-a" :: m :: tail if (m.matches( """0(\.\d*)?""")) =>
            parseCommandLineRec(tail, positional, option + ('associative -> m))
          case "-h" :: tail => Dispatcher.error(usage)
          case s :: tail if (s(0) == '-') => Dispatcher.error("Invalid switch " + s)
          case arg :: tail if (arg.matches( """\d+""")) => parseCommandLineRec(tail, arg :: positional, option)
          case arg :: tail => Dispatcher.error("Invalid argument " + arg)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      Dispatcher.errorIf(positional.size != 2, "Invalid number of arguments")
      val maxSearch = option.get('maxSearch) match {
        case Some(s) => s.toInt
        case None => defaultMaxSearch
      }
      val cageSize = option.get('cageDistribution) match {
        case Some(s) => Multinomial(s)
        case None => defaultCageSizeDistribution
      }
      val associativeProbability = option.get('associative) match {
        case Some(s) => s.toDouble
        case None => defaultAssociativeProbability
      }
      val specifiedProportion = option.get('specified) match {
        case Some(s) => s.toDouble
        case None => defaultSingleCellProportion
      }
      Dispatcher.errorIf(associativeProbability < 0 || associativeProbability > 1,
        "Invalid associative probability " + associativeProbability)
      (positional(0).toInt, positional(1).toInt, !option.contains('nonUnique), maxSearch,
        cageSize, specifiedProportion, associativeProbability)
    }

    def prepend(prefix: String, s: String) = s.split("\n").map(prefix + _).mkString("\n")

    def mapSum(m1: Map[Int, Int], m2: Map[Int, Int]) =
      Map() ++ (m1.keys ++ m2.keys).map(k => (k -> (m1.getOrElse(k, 0) + m2.getOrElse(k, 0))))

    def averagesTable(averages: Map[Int, Double]) =
      (1 to averages.keys.max).map(i => "%d: %.3f".format(i, averages.getOrElse(i, 0.0))).mkString("\n")

    val (numPuzzles, n, unique, maxSearch, cageSize, specifiedProportion, associativeProbability) =
      parseCommandLine(args)

    val puzzles = for (_ <- (1 to numPuzzles).toStream)
    yield if (unique)
        uniqueRandomPuzzle(n, cageSize, maxSearch, specifiedProportion, associativeProbability)
      else
        randomPuzzle(n, cageSize, specifiedProportion, associativeProbability)

    var macroCageSize = Map[Int, Int]()
    var totalCages = 0
    var totalDifficulty = 0
    for (((puzzle, solution), i) <- puzzles.zipWithIndex) {
      println("# " + (i + 1) + ".\n" + puzzle + "\n" + prepend("# ", tableToString(solution)))
      val difficulty = OracleSolver(solution).difficulty(puzzle)
      println("# Difficulty: " + difficulty + "\n")
      totalDifficulty += difficulty
      macroCageSize = mapSum(macroCageSize, puzzle.cageSizes)
      totalCages += puzzle.cageConstraints.size
    }
    println(prepend("# ", "Cage size, Single cell cage proportion, Associative probability\n%s, %.3f, %.3f".format(
      cageSize, specifiedProportion, associativeProbability)))
    println(prepend("# ", if (unique) "Unique solutions, maximum search " + maxSearch else "Non-unique solutions"))
    println("# Average difficulty: %.3f".format(totalDifficulty / numPuzzles.toDouble))
    println(prepend("# ", "Cage Size Macro Average:\n" +
      averagesTable(Map() ++ macroCageSize.map(kv => (kv._1 -> kv._2 / totalCages.toDouble)))))
  }
}
