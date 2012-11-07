package kenken

import scala._
import annotation.tailrec
import collection.mutable
import collection.SeqView
import scala.Predef._
import scala.Some
import java.io.FileReader


/**
 * A [[http://www.kenken.com KenKen]] puzzle.
 *
 * This is a set of constraints on a square grid of numbers
 * @param n the dimension of the grid
 * @param cageConstraints cage constraints in the puzzle
 */
class KenKen(n: Int, cageConstraints: List[Constraint] = Nil) {
  /**
   * Map of cells in the puzzle grid to the constraints that contain them
   */
  private val constraintMap = createConstraintMap(latinSquareConstraints(n) ::: cageConstraints)

  /**
   * A solution to the puzzle.
   * @return puzzle solution or `None` if it cannot be solved
   */
  def solution: Option[Grid] = solutions.take(1).toList match {
    case Nil => None
    case ss => Option(ss.head)
  }

  /**
   * All solutions to the puzzle.
   * @return a lazy sequence of all the grids that solve this puzzle
   */
  def solutions: SeqView[Grid, Seq[_]] = {
    // The search may turn up multiple copies of the same solution, so ensure
    // that this function returns a unique list.
    val visited = mutable.Set[Grid]()

    def search(grid: Grid, constraints: List[Constraint]): SeqView[Grid, Seq[_]] = {
      propagateConstraints(grid, Set(constraints: _*)) match {
        case None => Nil.view
        case Some(g) if (visited.contains(g)) => Nil.view
        case Some(g) if (g.isSolved) => {
          visited += g
          List(g).view
        }
        case Some(g: Grid) => unsolvedCells(g).flatMap {
          case (cell, values) =>
            values.flatMap(value => search(g + (cell -> Set(value)), constraintMap(cell)))
        }
      }
    }
    search(Grid(n), cageConstraints)
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
    (Map[(Int, Int), List[Constraint]]() /: cs) {
      case (m, (cell, constraint)) => m + (cell -> (constraint :: m.getOrElse(cell, Nil)))
    }
  }
}

object KenKen {
  def apply(n: Int, cageConstraints: List[Constraint] = Nil): KenKen = new KenKen(n, cageConstraints)

  def apply(s: String): KenKen = Parsers.parsePuzzle(s)

  def main(args: Array[String]) {
    val in: FileReader = new FileReader(args(0))
    val puzzle = Parsers.parsePuzzle(in)
    puzzle.solution match {
      case Some(solution) => println(solution)
      case None => println("The puzzle cannot be solved.")
    }
  }
}