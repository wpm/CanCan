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
      val constraint = """(\d+)([+-x*])?\s+(\w+)""".r
      lines.foldLeft(Map[String, (Int, String)]()) {
        (constraintMap, line) =>
          val constraint(m, operation, label) = line
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

  def main(args: Array[String]) {
    val p = Puzzle(2)
    println(p)

    val s =
      """a a b
        |a b b
        |c c d
        |5+ a
        |3x b
        |2- c
        |1  d""".stripMargin
    println(apply(s))
  }
}