package cancan

import annotation.tailrec


/**
 * Constraint on a region of cells in a grid
 * @param region region of cells in the grid
 */
abstract class Constraint(region: Seq[Cell]) extends ((Grid) => Option[Seq[(Cell, Set[Int])]]) {
  val cells = region.sorted

  /**
   * Apply the constraint to the grid
   * @return sequence of (cell, values) tuples for all changed cells or `None` if the constraint cannot be satisfied
   */
  override def apply(grid: Grid): Option[Seq[(Cell, Set[Int])]] = {
    val before = values(grid)
    constrainedValues(before) match {
      case None => None
      case Some(after) => Some(changedValues(cells, before, after))
    }
  }

  protected def changedValues(cells: Seq[Cell], before: Seq[Set[Int]], after: Seq[Set[Int]]): Seq[(Cell, Set[Int])] = {
    List(cells.zip(before), cells.zip(after)).transpose.filterNot(l => l(0)._2 == l(1)._2).map(_(1))
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
abstract class RowColumnConstraint(region: Seq[Cell]) extends Constraint(region) {
  protected def solvedValues(values: Seq[Set[Int]]) = values.filter(_.size == 1)

  override def toString() = {
    val (r, c) = (cells.head.row, cells.head.col)
    if (cells.forall(_.row == r))
      "Row " + r
    else {
      require(cells.forall(_.col == c), "Not a row or column constraint")
      "Col " + c
    }
  }
}

/**
 * All solved cells in the region must be unique.
 *
 * This constraint does not change any values in the grid but can be violated.
 *
 *  - `[123 123 123] -> [123 123 123]`
 *  - `[1   23  123] -> [1   23  123]`
 *  - `[1   23  1]   -> None`
 */
case class LatinSquareConstraint(region: Seq[Cell]) extends RowColumnConstraint(region) {
  private def isDistinct[T](s: Seq[T]) = s.size == s.distinct.size

  override protected def constrainedValues(values: Seq[Set[Int]]) =
    if (isDistinct(solvedValues(values))) Some(values) else None

  override def toString() = "Latin Square: " + super.toString
}

case class PermutationSetConstraint(n: Int, region: Seq[Cell]) extends RowColumnConstraint(region) {
  override protected def constrainedValues(values: Seq[Set[Int]]) = {
    val cs = valueCounts(values).filter {
      case (s, c) => s.size <= c && c != n
    }
    if (cs.exists {
      case (s, c) => s.size < c // e.g. 1 1 123
    }) None
    else {
      val ps = cs.map(_._1)
      val constrained = values.map(v => (v /: ps.filterNot(v == _))(_ -- _))
      if (constrained.exists(_.isEmpty)) None else Some(constrained)
    }
  }

  // Could keep a running tally of these counts in the Grid object so that they are only calculated as needed. However,
  // profiling shows <5% of time spent inside this function, so it's probably not worth complicating the code.
  def valueCounts(values: Seq[Set[Int]]): Map[Set[Int], Int] =
    (Map[Set[Int], Int]().withDefaultValue(0) /: values) {
      case (m, s) => m + (s -> (m(s) + 1))
    }

  override def toString() = "Permutation Set: " + super.toString
}


/**
 * If a value only appears in a single cell in the region, that cell is solved.
 *
 *  - `[12 23 23] -> [1 23 23]`
 */
case class UniquenessConstraint(region: Seq[Cell]) extends RowColumnConstraint(region) {

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

  override def toString() = "Uniqueness: " + super.toString
}


/**
 * A constraint parameterized by an integer value.
 */
abstract class CageConstraint(value: Int, region: Seq[Cell]) extends Constraint(region) {
  protected val symbol: String
  lazy protected val nekNekSymbol: String = symbol

  override def toString() = value + "" + symbol

  /**
   * String representation of the constraint in the format recognized by the
   * [[http://www.mlsite.net/neknek NekNek solver]].
   */
  def toNekNekString: String = {
    nekNekSymbol + "\t" + value + "\t" + cells.map(_.toNekNekString).mkString(" ")
  }
}

/**
 * A single cell contains a specified value
 * @param value the value the cell must contain
 * @param cell the cell
 */
case class SpecifiedConstraint(value: Int, cell: Cell) extends CageConstraint(value, Seq(cell)) {
  override def apply(grid: Grid): Option[Seq[(Cell, Set[Int])]] =
    if (grid(cell).contains(value)) Some(Seq(cell -> Set(value))) else None

  override protected val symbol = ""
  lazy override protected val nekNekSymbol = "!"
}

/**
 * A set of cells whose values must combine arithmetically to a specified value.
 */
abstract class ArithmeticConstraint(value: Int, region: Seq[Cell]) extends CageConstraint(value, region) {
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
abstract class NonAssociativeConstraint(value: Int, cell1: Cell, cell2: Cell)
  extends ArithmeticConstraint(value, Seq(cell1, cell2)) {

  def fills(values: Seq[Set[Int]]) =
    (for (a <- values.head; b <- values.last; if satisfied(a, b) || satisfied(b, a)) yield Seq(a, b)).toSeq

  /**
   * Does this pair of numbers satisfy the constraint?
   * @param x a number in a cell
   * @param y a number in a cell
   * @return `true` if the combination satisfies the constraint
   */
  def satisfied(x: Int, y: Int): Boolean
}

/**
 * The difference of a pair of cells must equal a specified value.
 */
case class MinusConstraint(value: Int, cell1: Cell, cell2: Cell) extends NonAssociativeConstraint(value, cell1, cell2) {
  def satisfied(x: Int, y: Int) = x - y == value

  override protected val symbol = "-"
}

/**
 * The quotient of a pair of cells must equal a specified value.
 */
case class DivideConstraint(value: Int, cell1: Cell, cell2: Cell) extends NonAssociativeConstraint(value, cell1, cell2) {
  def satisfied(x: Int, y: Int) = x % y == 0 && x / y == value

  override protected val symbol = "/"
}

/**
 * A set of cells whose values combine with an associative operator
 */
abstract class AssociativeConstraint(value: Int, region: Seq[Cell]) extends ArithmeticConstraint(value, region) {
  def fills(values: Seq[Set[Int]]) = cartesianMonoid(values)

  /**
   * Take the Cartesian product of a set of integers and select the elements whose combination on a monoid is equal
   * to a specified value.
   *
   * @param ys sets of integers to combine
   * @return list of lists of integers combining to the target value
   */
  private def cartesianMonoid(ys: Seq[Traversable[Int]]): List[List[Int]] = {
    @tailrec
    def cmRec(ys: Seq[Traversable[Int]], acc: List[(List[Int], Int)]): List[List[Int]] = ys match {
      case Nil => acc.filter(_._2 == value).map(_._1.reverse)
      case z :: zs => cmRec(zs, for (a <- acc; b <- z; c = combine(a._2, b); if (c <= value)) yield (b :: a._1, c))
    }
    cmRec(ys, List((Nil, identity)))
  }

  /**
   * Combine two values with this constraint's operator
   * @param x a value
   * @param y a value
   * @return either x+y or x*y
   */
  protected def combine(x: Int, y: Int): Int

  /**
   * The identity element of the constraint's operator
   */
  protected val identity: Int
}

/**
 * The sum of the values in a set of cells must equal a specified value.
 */
case class PlusConstraint(value: Int, region: Seq[Cell]) extends AssociativeConstraint(value, region) {
  override protected def combine(x: Int, y: Int) = x + y

  override protected val identity = 0

  override protected val symbol = "+"
}

/**
 * The sum of the values in a set of cells must equal a specified value.
 */
case class TimesConstraint(m: Int, cs: Seq[Cell]) extends AssociativeConstraint(m, cs) {
  override protected def combine(x: Int, y: Int) = x * y

  override protected val identity = 1

  override protected val symbol = "x"
  lazy override protected val nekNekSymbol = "*"
}

object Constraint {
  /**
   * Map of cells in a puzzle grid to the constraints that contain them
   */
  def constraintMap(constraints: Set[_ <: Constraint]): Map[Cell, Set[Constraint]] = {
    (Map[Cell, Set[Constraint]]() /:
      (for (constraint <- constraints; cell <- constraint.cells)
      yield (cell -> constraint))) {
      case (m, (cell, constraint)) => m + (cell -> (m.getOrElse(cell, Set()) + constraint))
    }
  }
}
