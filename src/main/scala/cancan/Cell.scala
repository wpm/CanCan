package cancan

/**
 * A position in a [[cancan.Markup]].
 *
 * Cells sort by row then column. Row and column values are 1-based.
 * @param row cell row
 * @param col cell column
 */
case class Cell(row: Int, col: Int) extends Ordered[Cell] {
  override def toString = "(%d,%d)".format(row, col)

  /**
   * String representation in the format recognized by the [[http://www.mlsite.net/neknek NekNek solver]].
   */
  def toNekNekString: String = {
    require(row < 10 && col < 10, "NekNek only supports puzzles up to 9x9")
    ('A'.toInt + row - 1).toChar + col.toString
  }

  def compare(that: Cell) = row - that.row match {
    case 0 => col - that.col
    case c => c
  }
}

object Cell {
  implicit def tupleToCell(tuple: (Int, Int)) = Cell(tuple._1, tuple._2)
}