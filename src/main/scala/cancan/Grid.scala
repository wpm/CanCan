package cancan

import scala.collection.GenTraversableOnce

/**
 * An ''n'' x ''n'' grid containing a puzzle solution
 *
 * A grid is a square array of [[cancan.Cell]]s. Each cell contains a set of numbers from 1 to ''n'' which may possibly
 * occupy that cell in a solution. Cells are referred to by pairs of 1-based coordinates where (1,1) is in the upper
 * left hand corner.
 */
case class Grid private(n: Int, g: Map[Cell, Set[Int]]) {
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
  def apply(implicit cell: Cell) = g(cell)

  /**
   * Assign a value to a cell
   *
   * This is called by the inline assignment operator.
   *
   * @param cell a cell in the grid
   * @param values values in the cell
   * @return updated grid
   */
  def update(implicit cell: Cell, values: Set[Int]) = Grid(n, g.updated(cell, values))

  /**
   * Specify the values for a cell
   * @param kv cell/values pair
   * @return new grid with the cell values set
   */
  def +(kv: (Cell, Set[Int])) = Grid(n, g + kv)

  /**
   * Specify the values for a set of cells
   * @param xs cell/values pairs
   * @return new grid with the cell values set
   */
  def ++(xs: GenTraversableOnce[(Cell, Set[Int])]) = Grid(n, g ++ xs)


  /**
   * Grid of numbers, delimited with commas if some of the cells could contain numbers with multiple digits
   */
  override def toString = {
    val delimiter = if (n < 10) "" else ","
    StringRepresentation.tableToString(for (row <- (1 to n)) yield {
      for (col <- 1 to n) yield g(Cell(row, col)).toSeq.sorted.mkString(delimiter)
    })
  }
}

object Grid {
  /**
   * Create a maximally ambiguous grid
   * @param n grid dimension
   * @return maximally ambiguous grid
   */
  def apply(n: Int) = {
    val init = for (r <- (1 to n); c <- (1 to n)) yield Cell(r, c) -> Set((1 to n): _*)
    new Grid(n, Map() ++ init)
  }

  /**
   * Create a grid from a square matrix of integer sets
   * @param matrix table of integer sets
   * @return corresponding grid
   */
  implicit def apply(matrix: Seq[Seq[Set[Int]]]): Grid = {
    val n = matrix.length
    require(matrix.tail.forall(_.length == n), StringRepresentation.tableToString(matrix) + "\nis not square.")
    val init = for ((row, r) <- matrix.zipWithIndex; (item, c) <- row.zipWithIndex) yield (Cell(r + 1, c + 1) -> item)
    new Grid(n, Map() ++ init)
  }

  /**
   * Convert an string matrix of numbers to a grid
   * @param s array of numbers
   * @return corresponding grid
   */
  implicit def apply(s: String): Grid = StringRepresentation.parseGrid(s: String)

  /**
   * Create a grid from a square matrix of integers
   * @param matrix table of integers
   * @return corresponding grid
   */
  implicit def integerMatrixToGrid(matrix: Seq[Seq[Int]]): Grid = matrix.map(row => row.map(Set(_)))

  /**
   * Cells in a row
   * @param n size of the grid
   * @param r row number
   * @return the cells in the row
   */
  def row(n: Int)(r: Int) = Seq((1 to n).map(Cell(r, _)): _*)

  /**
   * Cells in a column
   * @param n size of the grid
   * @param c column number
   * @return the cells in the column
   */
  def col(n: Int)(c: Int) = Seq((1 to n).map(Cell(_, c)): _*)
}
