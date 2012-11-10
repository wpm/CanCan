package kenken

import annotation.tailrec
import scala.util.Random._

/**
 * Generator of random KenKen puzzles
 */
object Generator {
  /**
   * Ratio of puzzle size to mean maximum cage size
   */
  private val alpha = 0.5
  /**
   * Probability of assigning an associative operator to a 2-cell cage
   */
  private val beta = 1 / 3.0

  /**
   * Generate a random KenKen puzzle and its solution
   * @param n puzzle size
   * @return (solution, puzzle) tuple
   */
  def randomPuzzle(n: Int): (Array[Array[Int]], KenKen) = {
    val solution = randomLatinSquare(n)
    val cages = randomCageLayout(n)
    (solution, KenKen(n, cages.map(cage => randomCageConstraint(solution, cage.toList))))
  }

  /**
   * Generate a random cage constraint from a cage and a solution grid
   */
  def randomCageConstraint(solution: Array[Array[Int]], cage: List[(Int, Int)]): Constraint = {
    def randomAssociativeConstraint(values: List[Int]) = {
      nextInt(2) match {
        case 0 => PlusConstraint(values.sum, cage)
        case _ => TimesConstraint(values.product, cage)
      }
    }

    def randomNonAssociativeConstraint(values: List[Int]) = {
      require(values.size == 2)
      val (smaller, larger) = (values.sorted.head, values.sorted.last)
      require(smaller != larger)
      shuffle(MinusConstraint(larger - smaller, cage.head, cage.last) ::
        (if (larger % smaller == 0) List(DivideConstraint(larger / smaller, cage.head, cage.last)) else Nil)).head
    }

    val values = cage.map {
      case (r, c) => solution(r - 1)(c - 1)
    }
    cage.size match {
      case 1 => SpecifiedConstraint(values.head, cage.head)
      case 2 => if (nextDouble <= beta) randomAssociativeConstraint(values) else randomNonAssociativeConstraint(values)
      case _ => randomAssociativeConstraint(values)
    }
  }

  def randomLatinSquare(n: Int): Array[Array[Int]] = {
    def rotate[E](xs: List[E]) = (xs.head :: xs.tail.reverse).reverse
    // Write a random permutation in the first row, generate successive rows
    // by rotation, then shuffle the rows.
    shuffle((List(shuffle((1 to n).toList)) /: (1 to n - 1)) {
      case (rows, _) => rotate(rows.head) :: rows
    }).map(_.toArray).toArray
  }

  /**
   * Randomly group adjacent cells in a grid into cages
   * @param n size of the grid
   * @return sets of cells in cages
   */
  def randomCageLayout(n: Int) = {

    /**
     * Create a random graph between adjacent cells in a grid
     */
    def randomUndirectedCellGraph = {
      /**
       * Given a cell, choose a random set of adjacent cells and create
       * symmetric edges between them.
       */
      def randomUndirectedEdges(cell: (Int, Int)) = {
        /**
         * Cells above, below, to the left and right of a cell
         */
        def adjacentCells = {
          for (x <- (-1 to 1);
               y <- (-1 to 1)
               if ((x == 0 || y == 0) && x != y)
          )
          yield (x + cell._1, y + cell._2)
        }.filter {
          case (r, c) => r > 0 && r <= n && c > 0 && c <= n
        }
        val cells = adjacentCells
        val randomAdjacent = shuffle(cells).take(nextInt(cells.size))
        randomAdjacent.flatMap(adjacent => List(cell -> adjacent, adjacent -> cell))
      }

      val cells = for (x <- (1 to n); y <- (1 to n)) yield (x, y)
      val edges = cells.flatMap(cell => randomUndirectedEdges(cell))
      val adjacency = ((Map() ++ cells.map(cell => cell -> Set[(Int, Int)]())) /: edges) {
        case (m, (s, d)) => m + (s -> (m(s) + d))
      }
      (cells, adjacency)
    }

    def samplePoisson(mean: Int) = {
      val lambda = math.exp(-mean)
      var k = 0
      var p = 1.0
      do {
        k += 1
        p *= nextDouble
      } while (p >= lambda)
      k - 1
    }

    val (cells, edges) = randomUndirectedCellGraph
    connectedComponents(cells, edges(_: (Int, Int)), samplePoisson((n * alpha).toInt))
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
        // TODO Why do I get a bug when I add .take(m - cs.size)?
        val as = fs.flatMap(adjacent(_)).filter(!vs.contains(_))
        connectedComponent(as, vs ++ as, cs ++ as)
      }
    }

    ((Set[N](), List[Set[N]]()) /: ns) {
      case ((vs, css), n) => if (!vs.contains(n)) {
        val (nvs, ncs) = connectedComponent(List(n), vs, Set[N](n))
        (vs ++ nvs, ncs :: css)
      } else (vs, css)
    }._2
  }

  def main(args: Array[String]) {
    val m = args(0).toInt
    val n = args(1).toInt
    for (_ <- (1 to m)) {
      val (solution, puzzle) = randomPuzzle(n)
      val expect = Grid(solution)
      println(puzzle + "\n\nExpect\n" + expect + "\n\nFound")
      require(puzzle.solutions.exists {
        found => println(found + "\n"); found == expect
      }, "Did not solve the puzzle.")
    }
  }
}
