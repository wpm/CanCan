package kenken

import annotation.tailrec
import scala.util.Random._

/**
 * Generator of random KenKen puzzles
 */
object Generator {
  def randomLatinSquare(n: Int): List[List[Int]] = {
    def rotate[E](xs: List[E]) = (xs.head :: xs.tail.reverse).reverse
    // Write a random permutation in the first row, generate successive rows
    // by rotation, then shuffle the rows.
    shuffle((List(shuffle((1 to n).toList)) /: (1 to n - 1)) {
      case (rows, _) => rotate(rows.head) :: rows
    })
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

    val (cells, edges) = randomUndirectedCellGraph
    // TODO Set a maximum connected component size?
    connectedComponents(cells, edges(_: (Int, Int)))
  }

  /**
   * Find the connected components in an undirected graph
   *
   * This function requires that the graph be undirected, so the relationship
   * between nodes defined by `adjacent` must be symmetric.
   * @param ns the nodes in the graph
   * @param adjacent a function from a node to all its adjacent nodes
   * @tparam N node type
   * @return list of sets of nodes in connected components
   */
  def connectedComponents[N](ns: Traversable[N], adjacent: N => Traversable[N]): List[Set[N]] = {
    /**
     * Build a connected component from a set of frontier nodes
     * @param fs frontier nodes
     * @param vs visited nodes
     * @param cs current connected component
     * @return (visited nodes, connected component) tuple
     */
    @tailrec
    def connectedComponent(fs: List[N], vs: Set[N], cs: Set[N]): (Set[N], Set[N]) = fs match {
      case Nil => (vs, cs)
      case _ => {
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
    def adjacent[N](edges: Map[N, List[N]])(n: N): List[N] = edges.getOrElse(n, Nil)
    val edges = Map('a -> List('b, 'c), 'b -> List('a, 'c), 'c -> List('a, 'b),
      'd -> List('e), 'e -> List('d), 'f -> Nil)
    val nodes = edges.keys ++ edges.values.flatMap(x => x)
    val cs = connectedComponents(nodes, adjacent(edges)(_: Symbol))
    println(cs)

    println(randomLatinSquare(9).mkString("\n"))

    println(randomCageLayout(6))
  }
}
