package kenken

import scala._
import annotation.tailrec


/**
 * A set of constraints on a square grid of numbers
 * @param n the dimension of the grid
 */
class Puzzle(n: Int, cageConstraints: List[Constraint] = Nil) {
  val size = n
  val constraints = createConstraintMap(latinSquareConstraints(size) ::: cageConstraints)

  def solve: List[Grid] = {
    def solveRec(grid: Grid, unapplied: List[Constraint]): List[Grid] = {
      propagateConstraints(grid, Set(unapplied: _*)) match {
        case None => Nil
        case Some(g) if (g.isSolved) => List(g)
        case Some(g: Grid) => g.cells.flatMap {
          cell =>
            g(cell).flatMap(value => solveRec(g + (cell -> Set(value)), constraints(cell)))
        }
      }
    }
    solveRec(Grid(n), cageConstraints)
  }

  @tailrec
  private def propagateConstraints(grid: Grid, unapplied: Set[Constraint]): Option[Grid] = {
    if (unapplied.isEmpty) Option(grid)
    else {
      val constraint = unapplied.head
      grid.constrain(constraint) match {
        case None => None
        case Some((g, cells)) =>
          propagateConstraints(
            g,
            unapplied ++ cells.flatMap(constraints(_)) - constraint
          )
      }
    }
  }

  override def toString = constraints.toString()

  private def latinSquareConstraints(n: Int) = {
    def row(r: Int) = List((1 to n).map((r, _)): _*)
    def col(c: Int) = List((1 to n).map((_, c)): _*)

    (for (i <- (1 to n)) yield
      DefinitenessConstraint(row(i)) ::
        UniquenessConstraint(row(i)) ::
        DefinitenessConstraint(col(i)) ::
        UniquenessConstraint(col(i)) ::
        Nil).flatten.toList
  }

  /**
   * Create a map of cells to the constraints that contain them.
   */
  private def createConstraintMap(constraints: List[Constraint]) = {
    val cs = for (constraint <- constraints; cell <- constraint.cells) yield (cell -> constraint)
    (Map[(Int, Int), List[Constraint]]() /: cs)((m, p) => m + (p._1 -> (p._2 :: m.getOrElse(p._1, Nil))))
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
          case "+" => PlusConstraint(m, cells)
          case "-" => {
            require(cells.length == 2)
            MinusConstraint(m, cells.head, cells.tail.head)
          }
          case "x" => TimesConstraint(m, cells)
          case "/" => {
            require(cells.length == 2)
            DivideConstraint(m, cells.head, cells.tail.head)
          }
          case _ => {
            require(cells.length == 1)
            SpecifiedConstraint(m, cells.head)
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


  //    2 1 4 3
  //    4 3 2 1
  //    1 4 3 2
  //    3 2 1 4
  val p3 = Puzzle( """a a b b
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
                     |h 3+""".stripMargin)


  def main(args: Array[String]) {
    val p1 = Puzzle(2, List(SpecifiedConstraint(1, (1, 1))))
    println(p1.solve)

    //    4 3 1 2
    //    3 1 2 4
    //    2 4 3 1
    //    1 2 4 3
    val p2 = Puzzle( """a b b b
                       |a c d d
                       |e c d f
                       |e e f f
                       |a 1-
                       |b 6+
                       |c 5+
                       |d 9+
                       |e 5+
                       |f 8+""".stripMargin)
    println(p2)
    //    println(p2.solve)


    println(p3)
    println(p3.solve)
  }
}