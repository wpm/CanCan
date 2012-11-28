package kenken

import collection.TraversableView
import annotation.tailrec
import io.Source
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq

/**
 * An algorithm that solves a KenKen puzzle.
 * @param puzzle the puzzle to solve
 */
abstract class Solver(puzzle: Puzzle) {
  /**
   * Map of cells in the puzzle grid to the constraints that contain them
   */
  val constraintMap: Map[Cell, Set[Constraint]]

  /**
   * All the possible solutions for this puzzle.
   */
  lazy val solutions: TraversableView[Grid, Traversable[_]] = {
    applyConstraints(Grid(puzzle.n), puzzle.cageConstraints) match {
      case Some(g) => search(g).filter(_.isSolved)
      case None => Traversable[Grid]().view
    }
  }

  private def search(grid: Grid): TraversableView[Grid, Traversable[_]] = {
    Traversable(grid).view ++ nextGrids(grid).flatMap(g => search(g))
  }

  private def nextGrids(grid: Grid): TraversableView[Grid, Traversable[_]] = {
    for {cell <- guessCell(grid).toTraversable.view
         value <- grid(cell)
         next <- applyConstraints(grid + (cell -> Set(value)), constraintMap(cell))}
    yield next
  }

  private def guessCell(grid: Grid): Option[Cell] = {
    val u = grid.unsolved
    if (u.isEmpty) None
    else Some(u.toSeq.map {
      case (cell, values) => (values.size, cell)
    }.min._2)
  }

  /**
   * Return grid with the specified constraints applied. Return `None` if the grid is inconsistent with the
   * constraints.
   */
  @tailrec
  private def applyConstraints(grid: Grid, constraints: Set[_ <: Constraint] = puzzle.cageConstraints): Option[Grid] = {
    if (constraints.isEmpty) Option(grid)
    else {
      val constraint = constraints.head
      constraint(grid) match {
        case Some(changes) => {
          val newGrid: Grid = grid ++ changes
          val triggeredConstraints = changes.flatMap {
            case (cell, _) => constraintMap(cell)
          }
          applyConstraints(newGrid, constraints ++ triggeredConstraints - constraint)
        }
        case None => None
      }
    }
  }

  // TODO Add isPossibleSolution(grid:Grid):Boolean as a debugging utility.

  /**
   * Utility to create row and column constraints for all row and columns in a puzzle
   * @param n the puzzle size
   * @param constraints function that creates constraints for a given row or column
   * @return row and column constraints for the entire puzzle
   */
  protected def rowColumnConstraints(n: Int, constraints: Seq[Cell] => Seq[Constraint]) = {
    for {i <- (1 to n)
         cells <- Seq(Grid.row(n)(i), Grid.col(n)(i))
         constraint <- constraints(cells)
    } yield constraint
  }
}

/**
 * Solver that doesn't use any heuristics.
 */
case class MinimalSolver(puzzle: Puzzle) extends Solver(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints(puzzle.n, (cells => Seq(LatinSquareConstraint(cells)))))
}

/**
 * Solver that uses the [[kenken.PermutationSetConstraint.`]] and [[kenken.UniquenessConstraint]] heuristics.
 */
case class HeuristicSolver2(puzzle: Puzzle) extends Solver(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints(puzzle.n,
        (cells => Seq(PermutationSetConstraint(puzzle.n, cells), UniquenessConstraint(cells)))))
}

/**
 * Solver that uses the [[kenken.LinearComplementConstraint]], [[kenken.SolvedCellsConstraint]] and
 * [[kenken.UniquenessConstraint]] heuristics.
 */
case class HeuristicSolver1(puzzle: Puzzle) extends Solver(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      linearComplementConstraints(puzzle) ++
      rowColumnConstraints(puzzle.n,
        (cells => Seq(SolvedCellsConstraint(cells), UniquenessConstraint(cells)))))

  private def linearComplementConstraints(puzzle: Puzzle): Set[LinearComplementConstraint] = {
    puzzle.cageConstraints.map {
      c: CageConstraint => LinearComplementConstraint(puzzle.n, c.cells)
    }
  }
}

object Solver {
  def main(args: Array[String]) {
    // Treat # as a comment delimiter and skip leading blank lines.
    val lines = Source.fromFile(args(0)).getLines().map(_.replaceAll("#.*", "").trim).dropWhile(_.isEmpty)
    val in = new PagedSeqReader(PagedSeq.fromLines(lines))
    StringRepresentation.parsePuzzles(in).foreach {
      puzzle: Puzzle =>
        println(puzzle + "\n")
        println(HeuristicSolver2(puzzle).solutions.mkString("\n\n") + "\n\n")
    }
  }
}