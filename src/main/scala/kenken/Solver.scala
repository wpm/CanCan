package kenken

import collection.TraversableView
import annotation.tailrec

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
   * Find all the puzzle solutions, abandoning the search after a maximum number of partial solutions.
   * @param max the maximum number of partial iterations
   * @return tuple (solutions, `true` if the entire space was searched)
   */
  def cappedSolutions(max: Int): (List[Grid], Boolean) = {
    val i = search.toIterator
    (i.take(max).filter(_.isSolved).toList, i.isEmpty)
  }

  /**
   * A solution of this puzzle. If there are multiple solutions one will be returned arbitrarily.
   */
  lazy val solution: Option[Grid] = solutions.headOption

  /**
   * All the solutions for this puzzle.
   */
  lazy val solutions: TraversableView[Grid, Traversable[_]] = search.filter(_.isSolved)

  /**
   * All the partial solutions of this puzzle, including the complete solutions.
   * @return all the partial solutions of this puzzle
   */
  def search: TraversableView[Grid, Traversable[_]] = {
    def searchRec(grid: Grid): TraversableView[Grid, Traversable[_]] = {
      Traversable(grid).view ++ nextGrids(grid).flatMap(g => searchRec(g))
    }

    applyConstraints(Grid(puzzle.n), puzzle.cageConstraints) match {
      case Some(g) => searchRec(g)
      case None => Traversable[Grid]().view
    }
  }

  private def nextGrids(grid: Grid): TraversableView[Grid, Traversable[_]] = {
    for {cell <- guessCell(grid).toTraversable.view
         value <- grid(cell)
         next <- applyConstraints(grid + (cell -> Set(value)), constraintMap(cell))}
    yield next
  }

  protected def guessCell(grid: Grid): Option[Cell] = {
    val u = grid.unsolved
    if (u.isEmpty) None
    else Some(u.toSeq.map {
      case (cell, values) => (values.size, cell)
    }.min._2)
  }

  /**
   * Apply the specified constraints to a grid
   * @param grid the grid to constrain
   * @param constraints the constraints to apply, by default the puzzle's cage constraints
   * @return a constrained grid or `None` if the grid is inconsistent with the constraints
   */
  @tailrec
  final def applyConstraints(grid: Grid, constraints: Set[_ <: Constraint] = puzzle.cageConstraints): Option[Grid] = {
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
   * Does the specified grid satisfy all the constraints in this puzzle?
   *
   * This function is provided as a debugging utility to check that the solver is returning the correct answers.
   * @param grid the grid to check
   * @return `true` if the grid satisfies the constrains, `false` if any constraints are violated
   */
  def isPossibleSolution(grid: Grid): Boolean = applyConstraints(grid, constraintMap.values.reduce(_ ++ _)) != None

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
 * Solver that uses the [[kenken.PermutationSetConstraint]] and [[kenken.UniquenessConstraint]] heuristics and sorts
 * guess cells by cage ambiguity.
 */
case class HeuristicSolver2(puzzle: Puzzle) extends HeuristicSolver(puzzle) {
  def cageAmbiguity(grid: Grid): Map[Constraint, Int] = {
    Map[Constraint, Int]().withDefaultValue(0) ++
      puzzle.containingCages.values.map(cage => cage -> (1 /: cage.cells.map(grid(_).size))(_ * _))
  }

  override protected def guessCell(grid: Grid): Option[Cell] = {
    val u = grid.unsolved
    if (u.isEmpty) None
    else {
      val cageSize = cageAmbiguity(grid)
      Some(u.toSeq.map {
        case (cell, values) =>
          (cageSize(puzzle.containingCages(cell)), values.size, cell)
      }.min._3)
    }
  }
}

/**
 * Solver that uses the [[kenken.PermutationSetConstraint]] and [[kenken.UniquenessConstraint]] heuristics.
 */
case class HeuristicSolver1(puzzle: Puzzle) extends HeuristicSolver(puzzle)

abstract class HeuristicSolver(puzzle: Puzzle) extends Solver(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints(puzzle.n,
        (cells => Seq(PermutationSetConstraint(puzzle.n, cells), UniquenessConstraint(cells)))))
}

object Solver {
  /**
   * Solve all the puzzles in a file. Validate the answers if a '-v' switch is provided.
   *
   * Treat # as a comment delimiter in the puzzle file and skip leading blank lines.
   */
  def main(args: Array[String]) {
    def parseCommandLine(args: Array[String]): (String, Boolean) = {
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Set[Symbol]): (List[String], Set[Symbol]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-v" :: tail => parseCommandLineRec(tail, positional, option + 'validate)
          case s :: tail if (s(0) == '-') => {
            println("Invalid switch " + s)
            sys.exit(-1)
          }
          case arg :: tail => parseCommandLineRec(tail, arg :: positional, option)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Set())
      require(positional.size == 1, "Incorrect number of arguments")
      (positional.head, option.contains('validate))
    }

    val (filename, validate) = parseCommandLine(args)
    StringRepresentation.parsePuzzles(StringRepresentation.readFile(filename)).zipWithIndex.foreach {
      case (puzzle, i) =>
        println((i + 1) + ".\n" + puzzle + "\n")
        val solver = HeuristicSolver2(puzzle)
        val validator = if (validate) Some(MinimalSolver(puzzle)) else None
        solver.solutions.foreach {
          solution =>
            println(solution)
            println(if (validate)
              validator.get.isPossibleSolution(solution) match {
                case true => "VALID\n"
                case false => "INVALID\n"
              }
            else "")
        }
        println()
    }
  }
}