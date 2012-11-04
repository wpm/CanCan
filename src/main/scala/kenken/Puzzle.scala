package kenken

import scala._
import annotation.tailrec
import collection.mutable
import collection.SeqView


/**
 * A set of constraints on a square grid of numbers
 * @param n the dimension of the grid
 */
class Puzzle(n: Int, cageConstraints: List[Constraint] = Nil) {
  /**
   * Map of cells in the puzzle grid to the constraints that contain them
   */
  private val constraintMap = createConstraintMap(latinSquareConstraints(n) ::: cageConstraints)

  /**
   * A solution to the puzzle.
   * @return puzzle solution or _None_ if it cannot be solved
   */
  def solution: Option[Grid] = allSolutions.take(1).toList match {
    case Nil => None
    case ss => Option(ss.head)
  }

  /**
   * All solutions to the puzzle.
   * @return a view of all the grids that solve this puzzle
   */
  def allSolutions: SeqView[Grid, Seq[_]] = {
    // The search may turn up multiple copies of the same solution, so ensure
    // that this function returns a unique list.
    val solutions = mutable.Set[Grid]()

    def solveRec(grid: Grid, constraints: List[Constraint]): SeqView[Grid, Seq[_]] = {
      propagateConstraints(grid, Set(constraints: _*)) match {
        case None => Nil.view
        case Some(g) if (solutions.contains(g)) => Nil.view
        case Some(g) if (g.isSolved) => {
          solutions += g
          List(g).view
        }
        case Some(g: Grid) => unsolvedCells(g).flatMap {
          case (cell, values) =>
            values.flatMap(value => solveRec(g + (cell -> Set(value)), constraintMap(cell)))
        }
      }
    }
    solveRec(Grid(n), cageConstraints)
  }

  /**
   * Propagate constraints to the grid
   */
  @tailrec
  private def propagateConstraints(grid: Grid, constraints: Set[Constraint]): Option[Grid] = {
    if (constraints.isEmpty) Option(grid)
    else {
      val constraint = constraints.head
      grid.constrain(constraint) match {
        case None => None
        case Some((g, cells)) =>
          propagateConstraints(
            g,
            constraints ++ cells.flatMap(constraintMap(_)) - constraint
          )
      }
    }
  }

  /**
   * Unsolved cells and their values in order of the number of possible values
   */
  private def unsolvedCells(grid: Grid) = {
    grid.filter {
      case (cell, values) => values.size > 1
    }.toList.sortWith {
      (a, b) => a._2.size.compare(b._2.size) < 0
    }.view
  }

  override def toString = constraintMap.toString()

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
    val n = """\s+""".r.split(lines.head).length
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
}