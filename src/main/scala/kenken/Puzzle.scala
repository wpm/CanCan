package kenken

import scala._
import collection.mutable


// TODO How to implement all this with immutable variables?

/**
 * A set of constraints on a square grid of numbers
 * @param n the dimension of the grid
 */
class Puzzle(n: Int) {
  val size = n
  val constraints = constraintMap(latinSquareConstraints(size))

  override def toString = constraints.toString()

  private def latinSquareConstraints(n: Int) = {
    def row(r: Int) = List((1 to n).map((r, _)): _*)
    def col(c: Int) = List((1 to n).map((_, c)): _*)

    var constraints: List[Constraint] = Nil
    for (i <- (1 to n)) {
      constraints = DefinitenessConstraint(row(i)) :: constraints
      constraints = UniquenessConstraint(row(i)) :: constraints
      constraints = DefinitenessConstraint(col(i)) :: constraints
      constraints = UniquenessConstraint(col(i)) :: constraints
    }
    constraints
  }

  private def constraintMap(constraints: List[Constraint]) = {
    val m = mutable.Map[(Int, Int), List[Constraint]]()
    for (constraint <- constraints; cell <- constraint) {
      m(cell) = constraint :: m.getOrElse(cell, List.empty)
    }
    Map[(Int, Int), List[Constraint]]() ++ m
  }
}

object Puzzle {
  def apply(n: Int) = new Puzzle(n)

  def main(args: Array[String]) {
    val p = Puzzle(2)
    println(p)
  }
}