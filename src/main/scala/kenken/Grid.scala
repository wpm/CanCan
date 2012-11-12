package kenken

import collection.GenTraversableOnce


/**
 * An ''n'' x ''n'' grid containing a puzzle solution
 *
 * A grid is a square array of ''cells''. Each cell contains a set of numbers from 1 to ''n'' which may possibly occupy
 * that cell in a solution. Cells are referred to by pairs of 1-based coordinates where (1,1) is in the upper left hand
 * corner.
 */
case class Grid private(n: Int, g: Map[(Int, Int), Set[Int]]) {
  /**
   * Is this grid solved?
   *
   * A grid is solved if all its cells contain a single value.
   */
  def isSolved: Boolean = g.values.forall(_.size == 1)

  /**
   * The unsolved cells in the grid
   * @return sequence of (cell, values) tuples
   */
  def unsolved = Seq() ++ g.filter(_._2.size > 1)

  /**
   * Get the values in a cell
   * @param cell a cell in the grid
   * @return values in the cell
   */
  def apply(cell: (Int, Int)) = g(cell)

  /**
   * Specify the values for a cell
   * @param kv cell/values pair
   * @return new grid with the cell values set
   */
  def +(kv: ((Int, Int), Set[Int])) = Grid(n, g + kv)

  /**
   * Specify the values for a set of cells
   * @param xs cell/values pairs
   * @return new grid with the cell values set
   */
  def ++(xs: GenTraversableOnce[((Int, Int), Set[Int])]) = Grid(n, g ++ xs)

  /**
   * Apply a constraint to the grid
   * @param constraint the constraint to apply
   * @return tuple of the new grid and a list of changed cells or `None` if the constraint cannot be satisfied
   */
  def constrain(constraint: Constraint): Option[(Grid, Vector[(Int, Int)])] = {
    /**
     * `scala> val xs = Vector(('a, 1), ('b, 2), ('c, 3)); val ys = Vector(('a, 1), ('b, 3), ('c, 4))`
     * `scala> tupleDiff(xs, ys) // Vector[(Symbol, Int)] = Vector(('b,3), ('c,4))`
     */
    def tupleDiff[A, B](xs: Vector[(A, B)], ys: Vector[(A, B)]): Vector[(A, B)] =
      xs.zip(ys).filter(p => p._1._2 != p._2._2).map(_._2)

    val before = constraint.cells.map(cell => (cell, g(cell)))
    val values = before.map(_._2)
    constraint(values) match {
      case None => None
      case after => {
        val changed = tupleDiff(before, constraint.cells.zip(after.get))
        Option((this ++ changed, changed.map(_._1)))
      }
    }
  }

  override def toString = {
    def centered(s: String, width: Int) = {
      val pad = (width - s.length) / 2
      ("%" + width + "s").format(" " * pad + s)
    }
    def widest = g.values.map(_.mkString("").length).max
    (1 to n).map(r => (1 to n).map {
      c => centered(g((r, c)).toList.sorted.mkString(""), widest)
    }.mkString(" ")).mkString("\n")
  }
}


object Grid {
  /**
   * Create an empty grid
   * @param n grid dimension
   * @return empty grid
   */
  def apply(n: Int) = {
    val init = for (r <- (1 to n); c <- (1 to n)) yield (r, c) -> Set((1 to n): _*)
    new Grid(n, Map() ++ init)
  }

  /**
   * Create a grid from a square array of integer sets
   * @param m array of integer sets
   * @return new grid
   */
  def apply(m: Array[Array[Set[Int]]]) = {
    val n = m.length
    // The array must be square.
    require(m.forall(_.length == n), "The grid is not square:\n" + m)
    val init = for ((row, r) <- m.zipWithIndex; (item, c) <- row.zipWithIndex) yield ((r + 1, c + 1), item)
    new Grid(n, Map() ++ init)
  }

  /**
   * Create a solved grid from a square array of integers
   * @param m array of integers
   * @return new grid
   */
  def apply(m: Array[Array[Int]]): Grid = {
    apply(m.map(_.map(Set(_))))
  }

  /**
   * Convert an string array of numbers to a grid
   * @param s array of numbers
   * @return new grid
   */
  def apply(s: String): Grid = Parsers.parseGrid(s: String)

  /**
   * Cells in a row
   * @param n size of the grid
   * @param r row number
   * @return the cells in the row
   */
  def row(n: Int)(r: Int) = Vector((1 to n).map((r, _)): _*)

  /**
   * Cells in a column
   * @param n size of the grid
   * @param c column number
   * @return the cells in the column
   */
  def col(n: Int)(c: Int) = Vector((1 to n).map((_, c)): _*)
}