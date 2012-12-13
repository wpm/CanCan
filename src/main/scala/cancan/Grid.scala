package cancan

import scala.collection.GenTraversableOnce
import java.lang.IllegalArgumentException
import scala.util.parsing.input.Reader

/**
 * An ''n'' x ''n'' grid containing a puzzle solution
 *
 * A grid is a square array of [[cancan.Cell]]s. Each cell contains a set of numbers from 1 to ''n'' which may possibly
 * occupy that cell in a solution. Cells are referred to by pairs of 1-based coordinates where (1,1) is in the upper
 * left hand corner.
 */
case class Grid private(n: Int, g: Vector[Vector[Set[Int]]]) {
  /**
   * Is this grid solved?
   *
   * A grid is solved if all its cells contain a single value.
   */
  def isSolved: Boolean = g.flatten.forall(_.size == 1)

  /**
   * The unsolved cells in the grid
   * @return (cell, values) tuples where the number of values is greater than 1
   */
  def unsolved =
    for (r <- (1 to n); c <- (1 to n); values = g(r - 1)(c - 1); if (values.size > 1)) yield (Cell(r, c), values)

  /**
   * Get the values in a cell
   * @param cell a cell in the grid
   * @return values in the cell
   */
  def apply(implicit cell: Cell) = g(cell.row - 1)(cell.col - 1)

  /**
   * Assign a value to a cell
   *
   * This is called by the inline assignment operator.
   *
   * @param cell a cell in the grid
   * @param values values in the cell
   * @return updated grid
   */
  def update(implicit cell: Cell, values: Set[Int]) =
    Grid(n, setCellValues(g, cell, values))

  /**
   * Specify the values for a cell
   * @param kv cell/values pair
   * @return new grid with the cell values set
   */
  def +(kv: (Cell, Set[Int])) = this(kv._1) = kv._2

  /**
   * Specify the values for a set of cells
   * @param xs cell/values pairs
   * @return new grid with the cell values set
   */
  def ++(xs: GenTraversableOnce[(Cell, Set[Int])]) = {
    Grid(n, (g /: xs) {
      case (m, (cell, values)) => setCellValues(m, cell, values)
    })
  }

  private def setCellValues(v: Vector[Vector[Set[Int]]], cell: Cell, values: Set[Int]): Vector[Vector[Set[Int]]] = {
    v.updated(cell.row - 1, v(cell.row - 1).updated(cell.col - 1, values))
  }

  /**
   * Grid of numbers, delimited with commas if some of the cells could contain numbers with multiple digits
   */
  override def toString = {
    val delimiter = if (n < 10) "" else ","
    tableToString(for (row <- (0 to n - 1)) yield {
      for (col <- 0 to n - 1; values = g(row)(col)) yield {
        if (values.isEmpty) "." else values.toSeq.sorted.mkString(delimiter)
      }
    })
  }
}

object Grid {

  /**
   * Parser of a string representation of a [[cancan.Grid]].
   */
  private object GridParser extends MultilineParser {
    // 12
    def cell: Parser[Set[Int]] = """\d+|\.""".r ^^ (ss => Set() ++ (ss match {
      case "." => Set.empty[Int]
      case s => s.toList.map(_.toString.toInt)
    }))

    // 12 12
    def row: Parser[List[Set[Int]]] = opt(inLineWhitespace) ~> rep1sep(cell, inLineWhitespace) <~ lineDelimiter

    // 12 12
    // 12 12
    def grid: Parser[Grid] = rep(row) ^^ (Grid(_))

    // 12 12
    // 12 12
    //
    // 123 123 123
    // 123 123 123
    // 123 123 123
    def grids: Parser[List[Grid]] = repsep(grid, rep1(eol)) <~ opt(rep(eol))

    implicit def parseGridsString(s: String) = parseAll(grids, s)

    implicit def parseGridsFile(r: Reader[Char]) = parseAll(grids, r)
  }

  /**
   * Create a maximally ambiguous grid
   * @param n grid dimension
   * @return maximally ambiguous grid
   */
  def apply(n: Int) = new Grid(n, Vector.tabulate(n, n)((_, _) => Set[Int]((1 to n): _*)))

  /**
   * Create a grid from a square matrix of integer sets
   * @param matrix table of integer sets
   * @return corresponding grid
   */
  implicit def apply(matrix: Seq[Seq[Set[Int]]]): Grid = {
    val n = matrix.length
    require(matrix.tail.forall(_.length == n), tableToString(matrix) + "\nis not square.")
    new Grid(n, Vector() ++ matrix.map(Vector() ++ _))
  }

  /**
   * Convert an string matrix of numbers to a grid
   * @param s array of numbers
   * @return corresponding grid
   */
  implicit def apply(s: String): Grid = GridParser.parseAll(GridParser.grid, s) match {
    case GridParser.Success(a, _) => a
    case e: GridParser.Failure => throw new IllegalArgumentException(e.toString())
  }

  /**
   * Read a set of grids from a file or string
   */
  def parseGrids(implicit r: GridParser.ParseResult[List[Grid]]) = r match {
    case GridParser.Success(a, _) => a
    case e: GridParser.Failure => throw new IllegalArgumentException(e.toString())
  }

  /**
   * Create a grid from a square matrix of integers
   *
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

