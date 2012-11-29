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
   * Generate a random KenKen puzzle and its solution
   * @param n puzzle size
   * @param cageSize distribution from which to sample cage sizes
   * @return (solution, puzzle) tuple
   */
  def randomPuzzle(n: Int, cageSize: Multinomial): (Seq[Seq[Int]], Puzzle) = {
    val solution = randomLatinSquare(n)
    // Keep generating cage layouts until we have one that doesn't have too many single-cell components.
    val cages = Iterator.continually(randomCageLayout(n, cageSize)).
      find(cages => cages.filter(cage => cage.size == 1).size < n * n * gamma).get
    // TODO Make randomCageLayout return sets.
    (solution, Puzzle(n, Set() ++ cages.map(cage => randomCageConstraint(solution, Seq() ++ cage))))
  }

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
   * Write a random permutation in the first row, generate successive rows by rotation, then shuffle the rows.
   */
  private def randomLatinSquare(n: Int): Seq[Seq[Int]] = {
    def rotate[E](xs: List[E]) = (xs.head :: xs.tail.reverse).reverse
    shuffle((List(shuffle((1 to n).toList)) /: (1 to n - 1)) {
      case (rows, _) => rotate(rows.head) :: rows
    })
  }

  /**
   * Randomly group adjacent cells in a grid into cages
   * @param n size of the grid
   * @param cageSize distribution from which to sample cage sizes
   * @return sets of cells in cages
   */
  def randomCageLayout(n: Int, cageSize: Multinomial): List[Set[Cell]] = {

    /**
     * Create a random graph between adjacent cells in a grid
     */
    def randomUndirectedCellGraph = {
      /**
       * Given a cell, choose a random set of adjacent cells and create
       * symmetric edges between them.
       */
      def randomUndirectedEdges(cell: Cell) = {
        /**
         * Cells above, below, to the left and right of a cell
         */
        def adjacentCells = {
          for (x <- (-1 to 1);
               y <- (-1 to 1)
               if ((x == 0 || y == 0) && x != y)
          )
          yield Cell(x + cell.row, y + cell.col)
        }.filter {
          case Cell(r, c) => r > 0 && r <= n && c > 0 && c <= n
        }
        val cells = adjacentCells
        val randomAdjacent = shuffle(cells).take(nextInt(cells.size))
        randomAdjacent.flatMap(adjacent => List(cell -> adjacent, adjacent -> cell))
      }

      // The grid cells are the nodes in the graph.
      val cells = for (x <- (1 to n); y <- (1 to n)) yield Cell(x, y)
      // Draw edges between randomly adjacent cells and collect them into an adjacency list.
      val edges = cells.flatMap(cell => randomUndirectedEdges(cell))
      val emptyAdjacency = Map[Cell, Set[Cell]]() ++ cells.map(cell => cell -> Set[Cell]())
      val adjacency = (emptyAdjacency /: edges) {
        case (m, (s, d)) => m + (s -> (m(s) + d))
      }
      (cells, adjacency)
    }

    val (cells, edges) = randomUndirectedCellGraph
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
  def connectedComponents[N](ns: Traversable[N], adjacent: N => Traversable[N], m: Int): List[Set[N]] = {
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

    // The fold memo is (visited nodes, List(connected component)).
    ((Set[N](), List[Set[N]]()) /: ns) {
      case ((vs, css), n) => if (!vs.contains(n)) {
        val (nvs, ncs) = connectedComponent(List(n), vs, Set[N](n))
        (vs ++ nvs, ncs :: css)
      } else (vs, css)
    }._2
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

  /**
   * Discrete multinomial probability distribution over the support of the integers greater than zero.
   */
  case class Multinomial(xs: Double*) {
    private val cdf = (1 to xs.size).map(xs.map(_ / xs.sum).slice(0, _).sum)

    def sample() = cdf.indexWhere(_ > nextDouble) + 1

    override def toString = cdf.map("%.3f".format(_)).zipWithIndex.map(t => t._2 + ":" + t._1).mkString(" ")
  }

  def main(args: Array[String]) {
    def prepend(s: String, prefix: String) = s.split("\n").map(prefix + _).mkString("\n")

    require(args.size == 2, "Invalid number of arguments")
    val puzzles = args(0).toInt
    val n = args(1).toInt
    val cageSize = Multinomial(0, 0.07, 0.47, 0.46)

    for (((solution, puzzle), i) <- (for (_ <- (1 to puzzles)) yield randomPuzzle(n, cageSize)).zipWithIndex)
      println("# " + (i + 1) + ".\n" + puzzle + "\n" +
        prepend(StringRepresentation.tableToString(solution), "# ") + "\n")
  }
}
