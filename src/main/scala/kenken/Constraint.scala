package kenken

/**
 * Constraint on a region of cells in a grid
 * @param region region of cells in the grid
 */
abstract class Constraint(region: Seq[(Int, Int)]) extends ((Grid) => Option[Seq[((Int, Int), Set[Int])]]) {
  val cells = region.sorted

  /**
   * Apply the constraint to the grid
   * @return sequence of (cell, values) tuples for all changed cells or `None` if the constraint cannot be satisfied
   */
  override def apply(grid: Grid): Option[Seq[((Int, Int), Set[Int])]] = {
    /**
     * {{{
     * val xs = Vector(('a, 1), ('b, 2), ('c, 3)); val ys = Vector(('a, 1), ('b, 3), ('c, 4))
     * tupleDiff(xs, ys) // Vector[(Symbol, Int)] = Vector(('b,3), ('c,4))
     * }}}
     */
    def tupleDiff[A, B](xs: Seq[(A, B)], ys: Seq[(A, B)]): Seq[(A, B)] =
      xs.zip(ys).filter(p => p._1._2 != p._2._2).map(_._2)

    val before = values(grid)
    constrainedValues(before) match {
      case None => None
      case Some(after) => Some(tupleDiff(cells.zip(before), cells.zip(after)))
    }
  }

  /**
   * Values in the cells
   * @param grid a grid
   * @return the values in the grid for this constraint's cells
   */
  protected def values(grid: Grid): Seq[Set[Int]] = cells.map(grid(_))

  /**
   * Changes this constraint makes to a set of cell values
   * @param values original values in the grid
   * @return constrained values or `None` if the constraint cannot be satisfied
   */
  protected def constrainedValues(values: Seq[Set[Int]]): Option[Seq[Set[Int]]] = None

  override def toString() = cells.mkString(" ")
}

/**
 * Constraint that applies to a row or column of a grid
 */
abstract class RowColumnConstraint(region: Seq[(Int, Int)]) extends Constraint(region) {
  override def toString() = {
    val (r, c) = (cells.head._1, cells.head._2)
    if (cells.forall(_._1 == r))
      "Row " + r
    else {
      require(cells.forall(_._2 == c), "Not a row or column constraint")
      "Col " + c
    }
  }
}

/**
 * All solved cells in the region must be unique.
 *
 * This constraint does not change any values in the grid but can be violated.
 *
 * - `[123 123 123] -> [123 123 123]`
 * - `[1   23  123] -> [1   23  123]`
 * - `[1   23  1]   -> None`
 */
case class LatinSquareConstraint(region: Seq[(Int, Int)]) extends RowColumnConstraint(region) {
  override protected def constrainedValues(values: Seq[Set[Int]]) =
    if (isDistinct(solvedValues(values))) Some(Seq[Set[Int]]()) else None

  private def solvedValues(values: Seq[Set[Int]]) = values.filter(_.size == 1)

  private def isDistinct[T](s: Seq[T]) = s.size == s.distinct.size
}

/**
 * Values from all solved cells in a are removed from all the other cells in the region.
 *
 * The constraint is violated if the solved values are not all distinct.
 * The [[kenken.LatinSquareConstraint]] is a subset of this one.
 *
 * - `[1 2 1234 234] -> [1 2 34 34]`
 * - `[1 1 123] -> None`
 */
case class SolvedCellsConstraint(region: Seq[(Int, Int)]) extends RowColumnConstraint(region) {
  override protected def constrainedValues(values: Seq[Set[Int]]) = {
    // Partition input into solved and non-solved and subtract the union of
    // the non-solved values from the solved.
    val d = values.filter(_.size == 1).foldLeft(List[Int]())((memo, x) => x.head :: memo)
    if (d.distinct != d)
      None
    else {
      //
      val s = Set() ++ d
      val ncs = values.map {
        value =>
          value.size match {
            case 1 => value
            case _ => value -- s
          }
      }
      if (ncs.exists(_.isEmpty)) None else Some(ncs)
    }
  }

  override def toString() = "Definite: " + super.toString
}

/**
 * If a value only appears in a single cell in the region, that cell is solved.
 *
 * - `[12 23 23] -> [1 23 23]`
 */
case class UniquenessConstraint(region: Seq[(Int, Int)]) extends RowColumnConstraint(region) {

  override protected def constrainedValues(values: Seq[Set[Int]]) = {
    Some(values.map {
      value =>
      // Values only appearing in this cell.
        val u = value -- (values.filter(y => !(y eq value)).reduceLeft(_ | _))
        u.size match {
          case 1 => u
          case _ => value
        }
    })
  }

  override def toString() = "Sole: " + super.toString
}


/**
 * A constraint parameterized by an integer value.
 */
abstract class CageConstraint(value: Int, region: Seq[(Int, Int)]) extends Constraint(region) {
  protected val symbol: String
  lazy protected val nekNekSymbol: String = symbol

  override def toString() = value + "" + symbol

  /**
   * String representation of the constraint in the format recognized by the
   * [[http://www.mlsite.net/neknek NekNek solver]].
   */
  def toNekNekString: String = {
    val nekNekCells = cells.map {
      case (r, c) => ('A'.toInt + r - 1).toChar + c.toString
    }
    nekNekSymbol + "\t" + value + "\t" + nekNekCells.mkString(" ")
  }
}

/**
 * A single cell contains a specified value
 * @param value the value the cell must contain
 * @param cell the cell
 */
case class SpecifiedConstraint(value: Int, cell: (Int, Int)) extends CageConstraint(value, Seq(cell)) {
  override def apply(grid: Grid): Option[Seq[((Int, Int), Set[Int])]] = if (grid(cell).contains(value)) Some(Seq(cell -> Set(value))) else None

  override protected val symbol = ""
  lazy override protected val nekNekSymbol = "!"
}

/**
 * A set of cells whose values must combine arithmetically to a specified value.
 */
abstract class ArithmeticConstraint(value: Int, region: Seq[(Int, Int)]) extends CageConstraint(value, region) {
  override protected def constrainedValues(values: Seq[Set[Int]]) = {
    val f = fills(values)
    if (f.isEmpty) None else Some(f.transpose.map(Set() ++ _))
  }

  /**
   * Values that can fill the cells.
   *
   * For example, a 2-cell +5 constraint might return `List(List(2, 3), List(3, 2), List(4, 1))`.
   *
   * @param values current cell values
   * @return lists of possible values to fill the cells
   */
  def fills(values: Seq[Set[Int]]): Seq[Seq[Int]]
}

/**
 * A pair of cells whose values combine with a non-associative operator.
 *
 * A non-associative constraint must apply to exactly two cells.
 * The constraint is satisfied if either ordering of the cells produces the specified value.
 */
abstract class NonAssociativeConstraint(value: Int, cell1: (Int, Int), cell2: (Int, Int))
  extends ArithmeticConstraint(value, Seq(cell1, cell2)) {

  def fills(values: Seq[Set[Int]]) =
    (for (a <- values.head; b <- values.last; if satisfied(a, b) || satisfied(b, a)) yield Seq(a, b)).toSeq

  /**
   * Does this pair of numbers satisfy the constraint?
   * @param x a number in a cell
   * @param y a number in a cell
   * @return _true_ if the combination satisfies the constraint
   */
  def satisfied(x: Int, y: Int): Boolean
}

/**
 * The difference of a pair of cells must equal a specified value.
 */
case class MinusConstraint(value: Int, cell1: (Int, Int), cell2: (Int, Int)) extends NonAssociativeConstraint(value, cell1, cell2) {
  def satisfied(x: Int, y: Int) = x - y == value

  override protected val symbol = "-"
}

/**
 * The quotient of a pair of cells must equal a specified value.
 */
case class DivideConstraint(value: Int, cell1: (Int, Int), cell2: (Int, Int)) extends NonAssociativeConstraint(value, cell1, cell2) {
  def satisfied(x: Int, y: Int) = x % y == 0 && x / y == value

  override protected val symbol = "/"
}

/**
 * A set of cells whose values combine with an associative operator
 */
abstract class AssociativeConstraint(value: Int, region: Seq[(Int, Int)]) extends ArithmeticConstraint(value, region) {
  def fills(values: Seq[Set[Int]]) = {
    def cartesianProduct[A](zs: Traversable[Traversable[A]]): Seq[Seq[A]] =
      zs.foldLeft(Seq(Seq.empty[A])) {
        (x, y) => for (a <- x.view; b <- y) yield a :+ b
      }
    // TODO values.map(_.filterNot(_ > value)) Since these are always monotonically increasing functions.
    cartesianProduct(values).filter(_.reduceLeft(combine) == value)
  }

  /**
   * Combine two values with this constraint's operator
   * @param x a value
   * @param y a value
   * @return either x+y or x*y
   */
  def combine(x: Int, y: Int): Int
}

/**
 * The sum of the values in a set of cells must equal a specified value.
 */
case class PlusConstraint(value: Int, region: Seq[(Int, Int)]) extends AssociativeConstraint(value, region) {
  def combine(x: Int, y: Int) = x + y

  override protected val symbol = "+"
}

/**
 * The sum of the values in a set of cells must equal a specified value.
 */
case class TimesConstraint(m: Int, cs: Seq[(Int, Int)]) extends AssociativeConstraint(m, cs) {
  def combine(x: Int, y: Int) = x * y

  override protected val symbol = "x"
  lazy override protected val nekNekSymbol = "*"
}

object Constraint {
  /**
   * Constraints for a Latin Square of size `n`
   */
  def latinSquareConstraints(n: Int) = {
    for {i <- (1 to n)
         cells <- Seq(Grid.row(n)(i), Grid.col(n)(i))
         constraint <- Seq(SolvedCellsConstraint(cells), UniquenessConstraint(cells))
    } yield constraint
  }
}
