package kenken

import scala.math.Ordering.Implicits._
import collection.immutable.TreeSet

/**
 * NxN grid of possible values
 * @param n dimension of the grid
 * @param g grid of possible values
 */
class Grid private(n: Int, g: Map[(Int, Int), TreeSet[Int]]) extends Iterable[((Int, Int), TreeSet[Int])] {
  val grid = g

  def ++(changed: ((Int, Int), TreeSet[Int])*) = new Grid(n, g ++ changed)

  def apply(cell: (Int, Int)) = g(cell)

  def solved = g.values.forall(_.size == 1)

  def iterator = cells.map(cell => (cell, g(cell))).iterator

  /**
   * List of cells in the grid sorted by the number of possible values and then by location.
   * @return cells in the grid
   */
  def cells = g.keys.toList.sortWith {
    (a, b) =>
      g(a).size.compare(g(b).size) match {
        case 0 => a < b
        case c => c < 0
      }
  }

  def unsolvedCells = filter(x => x._2.size != 1)

  override def equals(other: Any) = other match {
    case that: Grid => canEqual(that) && g == that.grid
    case _ => false
  }

  // TODO Implement canEqual

  override def hashCode() = g.hashCode()

  override def toString() = {
    def centered(s: String, width: Int) = {
      val pad = " " * ((width - s.length) / 2)
      pad + s + pad
    }
    def widest = g.values.map(_.mkString("").length).max
    (1 to n).map(r => (1 to n).map(c => centered(g((c, r)).mkString(""), widest)).mkString(" ")).mkString("\n")
  }
}

object Grid {
  def apply(n: Int) = {
    val init = for (r <- (1 to n); c <- (1 to n)) yield (r, c) -> TreeSet((1 to n): _*)
    new Grid(n, Map(init: _*))
  }


  def main(args: Array[String]) {
    val g = Grid(3)

    val x = g ++ ((1, 1) -> TreeSet(1))
    val y = g ++ ((1, 1) -> TreeSet(1))
    val z = g ++ ((1, 1) -> TreeSet(2))

    println(Set[Grid](x, y, z))
  }
}