package kenken

import scala._
import annotation.tailrec
import collection.mutable
import collection.SeqView
import scala.Predef._
import scala.Some


/**
 * A set of constraints on a square grid of numbers
 * @param n the dimension of the grid
 * @param cageConstraints cage constraints in the puzzle
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

    def search(grid: Grid, constraints: List[Constraint]): SeqView[Grid, Seq[_]] = {
      propagateConstraints(grid, Set(constraints: _*)) match {
        case None => Nil.view
        case Some(g) if (solutions.contains(g)) => Nil.view
        case Some(g) if (g.isSolved) => {
          solutions += g
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
    (Map[(Int, Int), List[Constraint]]() /: cs)((m, p) => m + (p._1 -> (p._2 :: m.getOrElse(p._1, Nil))))
  }
}

object Puzzle {
  def apply(n: Int, cageConstraints: List[Constraint] = Nil): Puzzle = new Puzzle(n, cageConstraints)

  def apply(s: String): Puzzle = Parsers.parsePuzzle(s)
}