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
class Solver(puzzle: Puzzle) {
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
      case (cell, values) => (values.size, cell)
    }.min._2

    def guessCellValues(values: Set[Int]) = values.map(values - _).toSeq.view

    def applyGuessToGrid(cell: (Int, Int), guess: Set[Int]) =
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
  private def applyConstraints(grid: Grid, constraints: Set[_ <: Constraint]): Option[Grid] = {
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

  /**
   * Map of cells in the puzzle grid to the constraints that contain them
   */
  val constraintMap = Constraint.constraintMap(puzzle.cageConstraints ++ Constraint.latinSquareConstraints(puzzle.n))

  // TODO add isPossibleSolution(grid:Grid):Booelan as a debugging utility?
}

object Solver {
  def apply(puzzle: Puzzle): Solver = new Solver(puzzle)

  def main(args: Array[String]) {
    // Treat # as a comment delimiter and skip leading blank lines.
    val lines = Source.fromFile(args(0)).getLines().map(_.replaceAll("#.*", "").trim).dropWhile(_.isEmpty)
    val in = new PagedSeqReader(PagedSeq.fromLines(lines))
    StringRepresentation.parsePuzzles(in).foreach {
      puzzle: Puzzle => println(puzzle + "\n\n" + Solver(puzzle).solutions.mkString("\n\n") + "\n\n")
    }
  }
}