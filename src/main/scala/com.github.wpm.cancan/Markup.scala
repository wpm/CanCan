package com.github.wpm.cancan

import scala.collection.GenTraversableOnce
import java.lang.IllegalArgumentException
import scala.util.parsing.input.Reader

/**
 * An ''n'' x ''n'' grid containing numbers that may appear in a puzzle's solution
 *
 * The markup is a square array of [[com.github.wpm.cancan.Cell]]s. Each cell contains a set of numbers from 1 to ''n''
 * which may possibly occupy that cell in a solution. Cells are referred to by pairs of 1-based coordinates where (1,1)
 * is in the upper left hand corner.
 */
case class Markup private(n: Int, markup: Vector[Vector[Set[Int]]]) {
  /**
   * A markup is a solution if all its cells contain a single value.
   */
  def isSolution: Boolean = markup.flatten.forall(_.size == 1)

  /**
   * The unsolved cells in the markup
   *
   * @return (cell, values) tuples where the number of values is greater than 1
   */
  def unsolved =
    for (r <- (1 to n); c <- (1 to n); values = markup(r - 1)(c - 1); if (values.size > 1)) yield (Cell(r, c), values)

  /**
   * Get the values in a cell
   *
   * @param cell a cell
   * @return values in the cell
   */
  def apply(implicit cell: Cell) = markup(cell.row - 1)(cell.col - 1)

  /**
   * Assign a value to a cell
   *
   * @param cell a cell
   * @param values values in the cell
   * @return updated markup
   */
  def update(implicit cell: Cell, values: Set[Int]) =
    Markup(n, setCellValues(markup, cell, values))

  /**
   * Specify the values for a cell
   *
   * @param kv cell/values pair
   * @return new markup with the cell values set
   */
  def +(kv: (Cell, Set[Int])) = this(kv._1) = kv._2

  /**
   * Specify the values for a set of cells
   *
   * @param xs cell/values pairs
   * @return new markup with the cell values set
   */
  def ++(xs: GenTraversableOnce[(Cell, Set[Int])]) = {
    Markup(n, (markup /: xs) {
      case (m, (cell, values)) => setCellValues(m, cell, values)
    })
  }

  private def setCellValues(v: Vector[Vector[Set[Int]]], cell: Cell, values: Set[Int]): Vector[Vector[Set[Int]]] = {
    v.updated(cell.row - 1, v(cell.row - 1).updated(cell.col - 1, values))
  }

  /**
   * Matrix of numbers, delimited with commas if some of the cells could contain numbers with multiple digits
   */
  override def toString = {
    val delimiter = if (n < 10) "" else ","
    matrixToString(for (row <- (0 to n - 1)) yield {
      for (col <- 0 to n - 1; values = markup(row)(col)) yield {
        if (values.isEmpty) "." else values.toSeq.sorted.mkString(delimiter)
      }
    })
  }
}

object Markup {

  /**
   * Parser of a string representation of a [[com.github.wpm.cancan.Markup]].
   */
  private object MarkupParser extends MultilineParser {
    // 12
    def cell: Parser[Set[Int]] = """\d+|\.""".r ^^ (ss => Set() ++ (ss match {
      case "." => Set.empty[Int]
      case s => s.toList.map(_.toString.toInt)
    }))

    // 12 12
    def row: Parser[List[Set[Int]]] = opt(inLineWhitespace) ~> rep1sep(cell, inLineWhitespace) <~ lineDelimiter

    // 12 12
    // 12 12
    def markup: Parser[Markup] = rep(row) ^^ (Markup(_))

    // 12 12
    // 12 12
    //
    // 123 123 123
    // 123 123 123
    // 123 123 123
    def markups: Parser[List[Markup]] = repsep(markup, rep1(eol)) <~ opt(rep(eol))

    implicit def parseMarkupsString(s: String) = parseAll(markups, s)

    implicit def parseMarkupsFile(r: Reader[Char]) = parseAll(markups, r)
  }

  /**
   * Create a maximally ambiguous markup
   *
   * @param n markup dimension
   * @return maximally ambiguous markup
   */
  def apply(n: Int) = new Markup(n, Vector.tabulate(n, n)((_, _) => Set[Int]((1 to n): _*)))

  /**
   * Create a markup from a square matrix of integer sets
   *
   * @param matrix markup of integer sets
   * @return corresponding markup
   */
  implicit def apply(matrix: Seq[Seq[Set[Int]]]): Markup = {
    val n = matrix.length
    require(matrix.tail.forall(_.length == n), matrixToString(matrix) + "\nis not square.")
    new Markup(n, Vector() ++ matrix.map(Vector() ++ _))
  }

  /**
   * Convert an string markup of numbers to a markup
   *
   * @param s array of numbers
   * @return corresponding markup
   */
  implicit def apply(s: String): Markup = MarkupParser.parseAll(MarkupParser.markup, s) match {
    case MarkupParser.Success(a, _) => a
    case e: MarkupParser.Failure => throw new IllegalArgumentException(e.toString())
  }

  /**
   * Read a set of markups from a file or string
   */
  def parseMarkups(implicit r: MarkupParser.ParseResult[List[Markup]]) = r match {
    case MarkupParser.Success(a, _) => a
    case e: MarkupParser.Failure => throw new IllegalArgumentException(e.toString())
  }

  /**
   * Create a markup from a square matrix of integers
   *
   * @param matrix matrix of integers
   * @return corresponding markup
   */
  implicit def integerMatrixToMarkup(matrix: Seq[Seq[Int]]): Markup = matrix.map(row => row.map(Set(_)))

  /**
   * Cells in a row
   *
   * @param n size of the puzzle
   * @param r row number
   * @return the cells in the row
   */
  def row(n: Int)(r: Int) = Seq((1 to n).map(Cell(r, _)): _*)

  /**
   * Cells in a column
   *
   * @param n size of the puzzle
   * @param c column number
   * @return the cells in the column
   */
  def col(n: Int)(c: Int) = Seq((1 to n).map(Cell(_, c)): _*)
}

