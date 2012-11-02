package kenken

import scala._
import collection.mutable


/**
 * A set of constraints on a square grid of numbers
 * @param n the dimension of the grid
 */
class Puzzle(n: Int, cageConstraints: List[Constraint] = Nil) {
  val size = n
  val constraints = constraintMap(latinSquareConstraints(size) ::: cageConstraints)

  def propagateConstraints(grid: Grid) = {
    var unverified = Set(constraints.values.flatten.toList: _*)
    var constrainedGrid = grid
    while (!unverified.isEmpty) {
      val constraint = unverified.head
      println(constraint + "\n" + constraint(constrainedGrid) + "\n\n")
      constraint(constrainedGrid) match {
        case None => unverified = Set.empty[Constraint]
        case Some((g, cells)) => {
          constrainedGrid = g
          unverified ++= cells.flatMap(constraints(_))
          unverified -= constraint
        }
      }
    }
    constrainedGrid
  }

  // TODO Debugging only: remove.
  def allC = constraints.values.flatten.toList.distinct

  def printC() {
    for ((x, i) <- allC.zipWithIndex) println(i + ": " + x)
  }

  override def toString = constraints.toString()

  // TODO How to implement all this with immutable variables?

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
  def apply(n: Int, cageConstraints: List[Constraint] = Nil): Puzzle = new Puzzle(n, cageConstraints)

  def apply(s: String): Puzzle = {
    def constraintGridFromLines(lines: List[String]) = {
      def stringToCell(r: Int, cells: Array[String]) = cells.zipWithIndex.map {
        case (cell, i) => cell ->(r, i + 1)
      }

      val cells = lines.map( """\s+""".r.split(_))
      val init = cells.zipWithIndex.flatMap {
        case (line, r) => stringToCell(r + 1, line)
      }
      init.foldLeft(Map[String, List[(Int, Int)]]()) {
        (m, p) =>
          val cs: List[(Int, Int)] = p._2 :: m.getOrElse(p._1, List[(Int, Int)]())
          m + (p._1 -> cs)
      }
    }

    def constraintMapFromLines(lines: List[String]) = {
      val constraint = """(\w+)\s+(\d+)([+-x/])?""".r
      lines.foldLeft(Map[String, (Int, String)]()) {
        (constraintMap, line) =>
          val constraint(label, m, operation) = line
          constraintMap + (label ->(m.toInt, operation))
      }
    }

    val lines = s.split("\n").toList
    val n = "\\s+".r.split(lines.head).length
    val constraintGrid = constraintGridFromLines(lines.take(n))
    val constraintMap = constraintMapFromLines(lines.drop(n))
    val cageConstraints = constraintGrid.map {
      case (label, cells) =>
        val (m, operation) = constraintMap(label)
        operation match {
          case "+" => PlusConstraint(cells, m)
          case "-" => {
            require(cells.length == 2)
            MinusConstraint(cells.head, cells.tail.head, m)
          }
          case "x" => TimesConstraint(cells, m)
          case "/" => {
            require(cells.length == 2)
            DivideConstraint(cells.head, cells.tail.head, m)
          }
          case _ => {
            require(cells.length == 1)
            SpecifiedConstraint(cells.head, m)
          }
        }
    }.toList
    Puzzle(n, cageConstraints)
  }

  val p = Puzzle(
    """a a a b
      |c a d b
      |c e d d
      |c e f f
      |a 12+
      |b 3+
      |c 7+
      |d 9+
      |e 5+
      |f 4+""".stripMargin)

  def main(args: Array[String]) {
    val p1 = Puzzle(2)
    println(p1)

    //    4 3 1 2
    //    3 1 2 4
    //    2 4 3 1
    //    1 2 4 3
    val p2 = """a b b b
               |a c d d
               |e c d f
               |e e f f
               |a 1-
               |b 6+
               |c 5+
               |d 9+
               |e 5+
               |f 8+""".stripMargin
    println(apply(p2))


    //    2 1 4 3
    //    4 3 2 1
    //    1 4 3 2
    //    3 2 1 4
    val p3 = """a a b b
               |c d d e
               |c f f g
               |c h h g
               |a 2/
               |b 1-
               |c 12x
               |d 1-
               |e 1
               |f 12x
               |g 2/
               |h 3+""".stripMargin
    println(apply(p3))
  }
}