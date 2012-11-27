package kenken

import collection.SeqView
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
   * Map of cells in the puzzle grid to the size of the cages that contain them
   */
  lazy val cageSize: Map[Cell, Int] =
    constraintMap.map {
      case (cell, constraints) => (cell, cageSize(constraints))
    }

  private def cageSize(constraints: Set[Constraint]): Int = {
    val cages = constraints.filter(_.isInstanceOf[CageConstraint])
    require(cages.size <= 1, "Multiple cages for cell:" + cages)
    if (cages.size == 1) cages.head.cells.size else 0
  }

  /**
   * A stream of all the grids that solve this puzzle.
   */
  lazy val solutions: Stream[Grid] = {
    applyConstraints(Grid(puzzle.n), puzzle.cageConstraints) match {
      case Some(g) => search(g).toStream.distinct
      case None => Stream.empty[Grid]
    }
  }

  /**
   * Search the space of possible solutions consistent with a given starting grid, applying constraints at each step.
   */
  private def search(grid: Grid = Grid(puzzle.n)): SeqView[Grid, Seq[_]] = {
    def leastAmbiguousCell = grid.unsolved.map {
      case (cell, values) => (values.size, cageSize(cell), cell)
    }.min._3

    def guessCellValues(values: Set[Int]) = values.map(values - _).toSeq.view

    def applyGuessToGrid(cell: Cell, guess: Set[Int]) =
      applyConstraints(grid + (cell -> guess), constraintMap(cell)).toSeq

    if (grid.isSolved)
      Vector(grid).view
    else {
      val cell = leastAmbiguousCell
      for {guess <- guessCellValues(grid(cell))
           newGrid <- applyGuessToGrid(cell, guess)
           solution <- search(newGrid)
      } yield solution
    }
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

  // TODO add isPossibleSolution(grid:Grid):Boolean as a debugging utility?

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
 * Solver that uses the [[kenken.LinearComplementConstraint]], [[kenken.SolvedCellsConstraint]] and
 * [[kenken.UniquenessConstraint]] heuristics.
 */
case class HeuristicSolver(puzzle: Puzzle) extends Solver(puzzle) {
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
        println(HeuristicSolver(puzzle).solutions.mkString("\n\n") + "\n\n")
    }
  }
}