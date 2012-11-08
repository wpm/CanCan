package kenken

import annotation.tailrec
import scala.util.Random._

/**
 * Generator of random KenKen puzzles
 */
object Generator {
  def randomLatinSquare(n: Int): List[List[Int]] = {
    // Write a random permutation in the first row, generate successive rows
    // by rotation, then shuffle the rows.
    def rotate[E](xs: List[E]) = (xs.head :: xs.tail.reverse).reverse
    shuffle((List(shuffle((1 to n).toList)) /: (1 to n - 1)) {
      case (rows, _) => rotate(rows.head) :: rows
    })
  }

  /**
   * Find the connected components in an directed graph
   * @param ns the nodes in the graph
   * @param adjacent a function from a node to all its adjacent nodes
   * @tparam N node type
   * @return list of sets of nodes in connected components
   */
  def connectedComponents[N](ns: Traversable[N], adjacent: N => List[N]): List[Set[N]] = {
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
        val (nvs, ncs) = connectedComponent(List(n), vs, Set[N]())
        (vs ++ nvs, ncs :: css)
      } else (vs, css)
    }._2
  }

  def main(args: Array[String]) {
    def adjacent[N](edges: Map[N, List[N]])(n: N): List[N] = edges.getOrElse(n, Nil)
    val edges = Map('a -> List('b, 'c), 'b -> List('a, 'c), 'c -> List('a, 'b), 'd -> List('e), 'e -> List('d))
    val nodes = edges.keys ++ edges.values.flatMap(x => x)
    val cs = connectedComponents(nodes, adjacent(edges)(_: Symbol))
    println(cs)

    println(randomLatinSquare(9).mkString("\n"))
  }
}
