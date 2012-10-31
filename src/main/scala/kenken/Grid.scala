package kenken

import collection.immutable.TreeSet
import scala.math.Ordering.Implicits._
import collection.GenTraversableOnce


class Grid private(n: Int, grid: Map[(Int, Int), TreeSet[Int]]) {

  def isSolved = grid.values.forall(_.size == 1)

  def unsolvedCells = iterator.filter(x => x._2.size != 1)

  def apply(key: (Int, Int)) = grid(key)

  def +(kv: ((Int, Int), TreeSet[Int])) = new Grid(n, grid + kv)

  def ++(xs: GenTraversableOnce[((Int, Int), TreeSet[Int])]) = new Grid(n, grid ++ xs)

  def iterator: Iterator[((Int, Int), TreeSet[Int])] = cells.map(cell => (cell, grid(cell))).iterator

  /**
   * Apply the constraint to a grid
   * @return list of cells and their corresponding new possible values or _None_ if the constraint is unsatisfiable
   */
  def constrain(constraint: Constraint): Option[List[((Int, Int), Set[Int])]] = constraint(cells.map(cell => grid(cell))) match {
    case None => None
    case cellValues => Option(cells.zip(cellValues.get))
  }

  /**
   * List of cells in the grid sorted by the number of possible values and then by location.
   * @return cells in the grid
   */
  private def cells = grid.keys.toList.sortWith {
    (a, b) =>
      grid(a).size.compare(grid(b).size) match {
        case 0 => a < b
        case c => c < 0
      }
  }

  override def toString = {
    def centered(s: String, width: Int) = {
      val pad = " " * ((width - s.length) / 2)
      pad + s + pad
    }
    def widest = grid.values.map(_.mkString("").length).max
    (1 to n).map(r => (1 to n).map(c => centered(grid((r, c)).mkString(""), widest)).mkString(" ")).mkString("\n")
  }
}

object Grid {
  def apply(n: Int) = {
    val init = for (r <- (1 to n); c <- (1 to n)) yield (r, c) -> TreeSet((1 to n): _*)
    new Grid(n, Map(init: _*))
  }

  /**
   * Convert an string array of numbers to a grid
   * @param s array of numbers
   * @return corresponding grid
   */
  def apply(s: String) = {
    def stringToCell(r: Int, cells: Array[String]) = cells.zipWithIndex.map {
      case (cell, i) => (r, i + 1) -> TreeSet[Int](cell.toList.map(_.toString.toInt): _*)
    }
    val cells = s.split("\n").map("\\s+".r.split(_))
    // All lines must contain the same number of cells.
    val n = cells.head.length
    println("n=" + n)
    require(cells.forall(_.length == n))
    val init = cells.zipWithIndex.flatMap {
      case (line, r) => stringToCell(r + 1, line)
    }
    new Grid(n, Map(init: _*))
  }

  def main(args: Array[String]) {
    val g = Grid(3)
    println(g)
    println(g(1, 1))
    println(g((1, 1)))
    println(g.iterator.toList)

    val g2 = Grid(2) ++ List((1, 1) -> TreeSet(1), (1, 2) -> TreeSet(2), (2, 1) -> TreeSet(3), (2, 2) -> TreeSet(4))
    println(g2)

    val s =
      """12 12
        |12 2""".stripMargin
    println(Grid(s))
  }
}