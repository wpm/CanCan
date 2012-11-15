package kenken

import collection.GenTraversableOnce

// TODO Add case class Cell(row:Int, col:Int)? Specify toString and ordering.

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
   * @return (cell, values) tuples where the number of values is greater than 1
   */
  def unsolved = g.filter(_._2.size > 1)

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
   * Grid of numbers, delimited with commas if some of the cells could contain numbers with multiple digits
   */
  override def toString = {
    val delimiter = if (n < 10) "" else ","
    StringRepresentation.tableToString(for (row <- (1 to n)) yield {
      for (col <- 1 to n) yield g((row, col)).toSeq.sorted.mkString(delimiter)
    })
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
   * Create a grid from a square table of integer sets
   * @param m table of integer sets
   * @return new grid
   */
  def apply(m: Seq[Seq[Set[Int]]]): Grid = {
    val n = m.length
    // The array must be square.
    require(m.forall(_.length == n), "The grid is not square:\n" + m)
    val init = for ((row, r) <- m.zipWithIndex; (item, c) <- row.zipWithIndex) yield ((r + 1, c + 1), item)
    new Grid(n, Map() ++ init)
  }

  /**
   * Convert an string array of numbers to a grid
   * @param s array of numbers
   * @return new grid
   */
  def apply(s: String): Grid = StringRepresentation.parseGrid(s: String)

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