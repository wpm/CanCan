package cancan

import collection.TraversableView
import annotation.tailrec

/**
 * An algorithm that solves a KenKen puzzle.
 * @param puzzle the puzzle to solve
 * @param hint a grid to start from, or a maximally ambiguous grid if `None` is specified
 */
abstract class Solver(puzzle: Puzzle, hint: Option[Grid]) {
  private val init = if (hint == None) Grid(puzzle.n) else hint.get

  /**
   * Map of cells in the puzzle grid to the constraints that contain them
   */
  val constraintMap: Map[Cell, Set[Constraint]]

  /**
   * All the solutions of the puzzle.
   */
  lazy val solutions: Stream[Grid] = search.filter(_.isSolved).toStream

  /**
   * All the solutions of the puzzle and the intermediary partial solutions encountered during the search.
   */
  lazy val partialSolutions: Stream[Grid] = search.toStream

  /**
   * All the solutions of the puzzle up to `max` partial solution states.
   *
   * This is used to abandon difficult to solve puzzles after a finite amount of time.
   *
   * @param max the maximum number of partial solutions to search
   * @return tuple (solutions, stream of partial solutions beyond the ones searched)
   */
  def cappedSolutions(max: Int): (Stream[Grid], Stream[Grid]) =
    (partialSolutions.take(max).filter(_.isSolved), partialSolutions.drop(max))

  /**
   * All the partial solutions of this puzzle, including the complete solutions.
   * @return all the partial solutions of this puzzle
   */
  def search: TraversableView[Grid, Traversable[_]] = {
    def searchRec(grid: Grid): TraversableView[Grid, Traversable[_]] = {
      Traversable(grid).view ++ nextGrids(grid).flatMap(g => searchRec(g))
    }

    applyConstraints(init, puzzle.cageConstraints) match {
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
case class MinimalSolver(puzzle: Puzzle, hint: Option[Grid] = None) extends Solver(puzzle, hint) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints(puzzle.n, (cells => Seq(LatinSquareConstraint(cells)))))
}

/**
 * Solver that uses the [[cancan.PermutationSetConstraint]] and [[cancan.UniquenessConstraint]] heuristics and sorts
 * guess cells by cage ambiguity.
 */
case class HeuristicSolver2(puzzle: Puzzle, hint: Option[Grid] = None) extends HeuristicSolver(puzzle, hint) {
  private def cageAmbiguity(grid: Grid): Map[Option[Constraint], Int] = {
    Map[Option[Constraint], Int]().withDefaultValue(0) ++
      puzzle.containingCages.values.map(cage => Some(cage) -> (1 /: cage.cells.map(grid(_).size))(_ * _))
  }

  override protected def guessCell(grid: Grid): Option[Cell] = {
    val u = grid.unsolved
    if (u.isEmpty) None
    else {
      val cageSize = cageAmbiguity(grid)
      Some(u.toSeq.map {
        case (cell, values) =>
          (cageSize(puzzle.containingCages.get(cell)), values.size, cell)
      }.min._3)
    }
  }
}

/**
 * Solver that uses the [[cancan.PermutationSetConstraint]] and [[cancan.UniquenessConstraint]] heuristics.
 */
case class HeuristicSolver1(puzzle: Puzzle, hint: Option[Grid] = None) extends HeuristicSolver(puzzle, hint)

abstract class HeuristicSolver(puzzle: Puzzle, hint: Option[Grid]) extends Solver(puzzle, hint) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints(puzzle.n,
        (cells => Seq(PermutationSetConstraint(puzzle.n, cells), UniquenessConstraint(cells)))))
}

object Solver {
  private val defaultAlgorithm = HeuristicSolver1

  /**
   * Solutions of a puzzle using the default solving algorithm
   * @param puzzle a puzzle
   * @param hint a grid to start from, or a maximally ambiguous grid if `None` is specified
   * @return the puzzle's solutions
   */
  def solutions(puzzle: Puzzle, hint: Option[Grid] = None): Stream[Grid] =
    defaultAlgorithm(puzzle, hint).solutions


  /**
   * All the solutions of the puzzle up to `max` partial solution states.
   *
   * This is used to abandon difficult to solve puzzles after a finite amount of time.
   *
   * @param max the maximum number of partial solutions to search
   * @return tuple (solutions, stream of partial solutions beyond the ones searched)
   */
  def cappedSolutions(puzzle: Puzzle, max: Int, hint: Option[Grid] = None): (Stream[Grid], Stream[Grid]) = {
    val partialSolutions = defaultAlgorithm(puzzle, hint).partialSolutions
    (partialSolutions.take(max).filter(_.isSolved), partialSolutions.drop(max))
  }

  private val usage =
    """solve [-a|-m|-v] file
      |
      |Solve the puzzles in a file.
      |
      |    -a - find all solutions instead of just the first one
      |    -m - maximum number of partial solutions to search before giving up
      |    -v - validate the returned solutions""".stripMargin

  /**
   * Solve all the puzzles in a file.
   */
  def solve(args: Array[String]) {
    def parseCommandLine(args: Array[String]): (String, Boolean, Boolean, Option[Int]) = {
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Map[Symbol, String]): (List[String], Map[Symbol, String]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-a" :: tail => parseCommandLineRec(tail, positional, option + ('all -> ""))
          case "-m" :: m :: tail if (m.matches( """\d+""")) =>
            parseCommandLineRec(tail, positional, option + ('max -> m))
          case "-v" :: tail => parseCommandLineRec(tail, positional, option + ('validate -> ""))
          case "-h" :: tail => CanCan.error(usage)
          case s :: tail if (s(0) == '-') => CanCan.error("Invalid switch " + s)
          case arg :: tail => parseCommandLineRec(tail, arg :: positional, option)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      CanCan.errorIf(positional.size != 1, "Incorrect number of arguments")
      val max = option.get('max) match {
        case Some(s) => Some(s.toInt)
        case None => None
      }
      (positional.head, !option.contains('all), option.contains('validate), max)
    }

    val (filename, firstOnly, validate, max) = parseCommandLine(args)
    StringRepresentation.parsePuzzles(StringRepresentation.readFile(filename)).zipWithIndex.foreach {
      case (puzzle, i) =>
        println((i + 1) + ".\n" + puzzle + "\n")
        val solver = defaultAlgorithm(puzzle)
        val validator = if (validate) Some(MinimalSolver(puzzle)) else None
        val (solutions, remaining) = max match {
          case Some(m) => solver.cappedSolutions(m)
          case None => (solver.solutions, Stream.Empty)
        }
        for (solution <- (if (firstOnly) solutions.headOption.toStream else solutions)) {
          println(solution)
          println(if (validate)
            validator.get.isPossibleSolution(solution) match {
              case true => "VALID\n"
              case false => "INVALID\n"
            }
          else "")
        }
        println(if (remaining.isEmpty) "" else "...\n")
    }
  }
}