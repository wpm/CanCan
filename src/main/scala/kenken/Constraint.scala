package kenken

/**
 * A constraint on the possible values of a set of cells in a grid.
 * @param cs the cells to which the constraint applies
 */
abstract class Constraint(cs: Vector[(Int, Int)]) {
  /**
   * The cells to which this constraint applies
   */
  val cells = cs.sorted

  /**
   * Apply the constraint to the values in a set of cells
   * @param xs sets of values in the cells
   * @return sets of values in the cells with the constraint applied or
   *         `None` if the constraint cannot be satisfied
   */
  def apply(xs: Vector[Set[Int]]): Option[Vector[Set[Int]]]

  override def toString = cells.mkString(" ")
}

/**
 * A row or column where all the values must be unique
 */
abstract class LineConstraint(cs: Vector[(Int, Int)]) extends Constraint(cs) {
  override def toString = {
    val (r, c) = (cs.head._1, cs.head._2)
    if (cs.forall(_._1 == r))
      "Row " + r
    else
      "Col " + c
  }
}

/**
 * All solved cells contain distinct values.
 *
 * The constraint is violated if the solved values are not all distinct.
 */
case class DefinitenessConstraint(cs: Vector[(Int, Int)]) extends LineConstraint(cs) {
  def apply(xs: Vector[Set[Int]]) = {
    // Partition input into solved and non-solved and subtract the union of
    // the non-solved values from the solved.
    val d = xs.filter(_.size == 1).foldLeft(List[Int]())((memo, x) => x.head :: memo)
    if (d.distinct != d)
      None
    else {
      //
      val s = Set(d: _*)
      Some(xs.map {
        x =>
          x.size match {
            case 1 => x
            case _ => x -- s
          }
      })
    }
  }

  override def toString = "Definite: " + super.toString
}

/**
 * If a value only appears in one cell, that cell is solved.
 */
case class SoleValueConstraint(cs: Vector[(Int, Int)]) extends LineConstraint(cs) {
  def apply(xs: Vector[Set[Int]]) = {
    Some(xs.map {
      x =>
        val u = x -- (xs.filter(y => !(y eq x)).reduceLeft(_ | _))
        u.size match {
          case 1 => u
          case _ => x
        }
    })
  }

  override def toString = "Sole: " + super.toString
}

/**
 * A constraint parameterized by an integer value.
 */
abstract class CageConstraint(m: Int, cs: Vector[(Int, Int)]) extends Constraint(cs) {
  /**
   * Short name of constraint that contains the parameter but not the list of cells
   */
  def cageName: String

  override def toString = cageName + ": " + super.toString
}

// TODO Specified is a degenerate case of Subset. Eliminate shared values in subsets within same row/column.
/**
 * The value of a single cell is specified.
 * @param m the value
 * @param cell the cell to which the constraint applies
 */
case class SpecifiedConstraint(m: Int, cell: (Int, Int)) extends CageConstraint(m, Vector(cell)) {
  def apply(xs: Vector[Set[Int]]) = if (xs.head.contains(m)) Some(Vector(Set(m))) else None

  override def cageName = m.toString
}


/**
 * A set of cells whose values must combine arithmetically to a specified value.
 */
abstract class ArithmeticConstraint(m: Int, cs: Vector[(Int, Int)]) extends CageConstraint(m, cs) {
  def apply(xs: Vector[Set[Int]]) = {
    val f = fills(xs)
    if (f.isEmpty) None else Some(f.transpose.map(Set(_: _*)))
  }

  /**
   * Values that can fill the cells.
   *
   * For example, a 2-cell +5 constraint might return `Vector(Vector(2, 3), Vector(3, 2), Vector(4, 1))`.
   *
   * @param xs cell possible values
   * @return lists of possible values to fill the cells
   */
  def fills(xs: Vector[Set[Int]]): Vector[Vector[Int]]
}

/**
 * A pair of cells whose values combine with a non-associative operator.
 *
 * A non-associative constraint must apply to exactly two cells.
 * The constraint is satisfied if either ordering of the cells produces the specified value.
 */
abstract class NonAssociativeConstraint(m: Int, c1: (Int, Int), c2: (Int, Int))
  extends ArithmeticConstraint(m, Vector(c1, c2)) {

  def fills(xs: Vector[Set[Int]]) =
    Vector() ++ (for (a <- xs.head; b <- xs.tail.head; if satisfied(a, b) || satisfied(b, a)) yield Vector(a, b))

  /**
   * Does this pair of numbers satisfy the constraint?
   * @param x a number in a cell
   * @param y a number in a cell
   * @return _true_ if the combination satisfies the constraint
   */
  def satisfied(x: Int, y: Int): Boolean
}

/**
 * The difference of a pair of cells must be a specified value.
 */
case class MinusConstraint(m: Int, c1: (Int, Int), c2: (Int, Int)) extends NonAssociativeConstraint(m, c1, c2) {
  def satisfied(x: Int, y: Int) = x - y == m

  override def cageName = m + "-"
}

/**
 * The quotient of a pair of cells must be a specified value.
 */
case class DivideConstraint(m: Int, c1: (Int, Int), c2: (Int, Int)) extends NonAssociativeConstraint(m, c1, c2) {
  def satisfied(x: Int, y: Int) = x % y == 0 && x / y == m

  override def cageName = m + "/"
}

/**
 * A set of cells whose values combine with an associative operator
 */
abstract class AssociativeConstraint(m: Int, cs: Vector[(Int, Int)]) extends ArithmeticConstraint(m, cs) {
  def fills(xs: Vector[Set[Int]]) = {
    def cartesianProduct[A](zs: Traversable[Traversable[A]]): Seq[Seq[A]] =
      zs.foldLeft(Seq(Seq.empty[A])) {
        (x, y) => for (a <- x.view; b <- y) yield a :+ b
      }
    Vector() ++ cartesianProduct(xs).filter(_.reduceLeft(combine) == m).map(Vector() ++ _)
  }

  def combine(x: Int, y: Int): Int
}

/**
 * The sum of the values in a set of cells must equal a specified value.
 */
case class PlusConstraint(m: Int, cs: Vector[(Int, Int)]) extends AssociativeConstraint(m, cs) {
  def combine(x: Int, y: Int) = x + y

  override def cageName = m + "+"
}

/**
 * The sum of the values in a set of cells must equal a specified value.
 */
case class TimesConstraint(m: Int, cs: Vector[(Int, Int)]) extends AssociativeConstraint(m, cs) {
  def combine(x: Int, y: Int) = x * y

  override def cageName = m + "x"
}

object Constraint {
  /**
   * Constraints for a Latin Square of size `n`
   */
  def latinSquareConstraints(n: Int) = {
    for {i <- (1 to n)
         cells <- Vector(Grid.row(n)(i), Grid.col(n)(i))
         constraint <- Vector(DefinitenessConstraint(cells), SoleValueConstraint(cells))
    } yield constraint
  }
}