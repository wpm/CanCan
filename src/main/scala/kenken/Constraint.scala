package kenken

import collection.immutable.TreeSet
import math.abs

abstract class Constraint(cs: List[(Int, Int)],
                          constraint: List[Set[Int]] => Option[List[Set[Int]]],
                          name: String) extends Iterable[(Int, Int)] {
  val cells = cs.sorted

  def iterator = cells.iterator

  def apply(xs: List[Set[Int]]) = constraint(xs)

  /**
   * Apply the constraint to a grid
   * @param grid grid of possible values
   * @return list of cells and their corresponding new possible values or _None_ if the constraint is unsatisfiable
   */
  def constrain(grid: Grid): Option[List[((Int, Int), Set[Int])]] = constraint(cells.map(cell => grid(cell))) match {
    case None => None
    case cellValues => Option(cells.zip(cellValues.get))
  }

  override def toString() = name + ": " + cells.mkString(" ")
}

class CageConstraint(cs: List[(Int, Int)],
                     m: Int, constraint: Int => List[Set[Int]] => Option[List[Set[Int]]],
                     operator: String)
  extends Constraint(cs, constraint(m), m + "" + operator)

case class UniquenessConstraint(cs: List[(Int, Int)]) extends Constraint(cs, Constraint.unique, "Unique")

case class DefinitenessConstraint(cs: List[(Int, Int)]) extends Constraint(cs, Constraint.definite, "Definite")

case class PlusConstraint(cs: List[(Int, Int)], m: Int) extends CageConstraint(cs, m, Constraint.plus, "+")

case class MinusConstraint(cs: List[(Int, Int)], m: Int) extends CageConstraint(cs, m, Constraint.minus, "-")

case class TimesConstraint(cs: List[(Int, Int)], m: Int) extends CageConstraint(cs, m, Constraint.times, "x")

case class DivideConstraint(cs: List[(Int, Int)], m: Int) extends CageConstraint(cs, m, Constraint.divide, "/")

object Constraint {
  /**
   * All solved cells are distinct.
   */
  def definite(xs: List[Set[Int]]) = {
    // Partition input into solved and non-solved and subtract the union of
    // the non-solved values from the solved. The constraint is violated if
    // the solved values are not all distinct.
    val d = xs.filter(_.size == 1).foldLeft(List[Int]())((memo, x) => x.head :: memo)
    if (d.distinct != d)
      None
    else {
      //
      val s = TreeSet(d: _*)
      Some(xs.map {
        x =>
          x.size match {
            case 1 => x
            case _ => x -- s
          }
      })
    }
  }


  /**
   * If a value only appears in one cell, that cell is solved.
   */
  def unique(xs: List[Set[Int]]) = {
    def union(xs: List[Set[Int]]) = xs.foldLeft(Set[Int]())((memo, x) => memo ++ x)

    val uniqueElements = union(xs.map(x => x -- union(xs.filter(_ != x))))
    val nonUniqueElements = union(xs) -- uniqueElements
    Some(xs.map {
      x =>
        val d = x -- nonUniqueElements
        if (d.isEmpty) x else d
    })
  }

  def plus(n: Int)(xs: Traversable[Traversable[Int]]) = arithmeticFilter(plusFilter)(n, xs)

  def times(n: Int)(xs: Traversable[Traversable[Int]]) = arithmeticFilter(timesFilter)(n, xs)

  def minus(n: Int)(xs: Traversable[Traversable[Int]]) = arithmeticFilter(minusFilter)(n, xs)

  def divide(n: Int)(xs: Traversable[Traversable[Int]]) = arithmeticFilter(divideFilter)(n, xs)

  def arithmeticFilter(f: (Int, Traversable[Traversable[Int]]) => Seq[Seq[Int]])
                      (n: Int, xs: Traversable[Traversable[Int]]) =
    f(n, xs).transpose match {
      case Nil => None
      case ys => Some(ys.map(TreeSet(_: _*)).toList)
    }

  def plusFilter(n: Int, xs: Traversable[Traversable[Int]]) = associative(n, xs)(_ + _)

  def timesFilter(n: Int, xs: Traversable[Traversable[Int]]) = associative(n, xs)(_ * _)

  def associative[A](n: A, xs: Traversable[Traversable[A]])(f: (A, A) => A) =
    cartesianProduct(xs).filter(_.reduceLeft(f) == n)

  def cartesianProduct[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])) {
      (x, y) => for (a <- x.view; b <- y) yield a :+ b
    }

  def nonAssociative[A](xs: Traversable[A], ys: Traversable[A])(f: ((A, A)) => Boolean) =
    xs.flatMap(x => ys.flatMap(y => List(List(x, y), List(y, x)))).toList.distinct.filter {
      p => f(p.head, p.tail.head) || f(p.tail.head, p.head)
    }

  def minusFilter(n: Int, xs: Traversable[Traversable[Int]]) =
    nonAssociative(xs.head, xs.tail.head)(p => abs(p._1 - p._2) == n)

  def divideFilter(n: Int, xs: Traversable[Traversable[Int]]) =
    nonAssociative(xs.head, xs.tail.head)(p => p._1 % p._2 == 0 && p._1 / p._2 == n)

  def main(args: Array[String]) {
    var r = List(TreeSet(1), TreeSet(2, 3), TreeSet(1, 2, 3))
    println(definite(r))
    r = List(TreeSet(1), TreeSet(1), TreeSet(1, 2, 3))
    println(definite(r))
    r = List(TreeSet(1), TreeSet(2, 3, 4), TreeSet(2, 3), TreeSet(2, 3))
    println(unique(r))

    println("5+ " + plus(5)(List(TreeSet(1, 2, 3, 4), TreeSet(1, 2, 3))))
    println("50+ " + plus(50)(List(TreeSet(1, 2, 3, 4), TreeSet(1, 2, 3, 4))))

    println("2- " + minus(2)(List(TreeSet(1, 2, 3, 4), TreeSet(1, 2, 3, 4))))
    println("20- " + minus(20)(List(TreeSet(1, 2, 3, 4), TreeSet(1, 2, 3, 4))))

    val r1d = DefinitenessConstraint(List((1, 1), (1, 2)))
    println(r1d)
    val r1u = UniquenessConstraint(List((1, 1), (1, 2)))
    println(r1u)
    val r1p = PlusConstraint(List((1, 1), (1, 2)), 5)
    println(r1p)
    val r1m = MinusConstraint(List((1, 1), (1, 2)), 1)
    println(r1m)

    val s =
      """123 123 123
        |123 123 123
        |123 123 123""".stripMargin
    val g3 = Grid(s)
    val gc = r1p.constrain(g3)
    println(g3)
    println(gc)
  }
}
