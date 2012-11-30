package kenken

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
   * Find all the puzzle solutions, abandoning the search after a maximum number of partial solutions.
   * @param max the maximum number of partial iterations
   * @return tuple (solutions, `true` if the entire space was searched)
   */
  def cappedSolutions(max: Int): (List[Grid], Boolean) = {
    val i = search.toIterator
    (i.take(max).filter(_.isSolved).toList, i.isEmpty)
  }

  /**
   * Find a puzzle solution, abandoning the search after a maximum number of partial solutions.
   *
   * This function will only return `true` if the puzzle had a unique solution and we found it.
   *
   * If there are multiple solutions one will be returned arbitrarily.
   * @param max the maximum number of partial iterations
   * @return tuple (optional solution, `true` if the entire space was searched)
   */
  def cappedSolution(max: Int): (Option[Grid], Boolean) = {
    val i = search.toIterator
    val solutions = i.take(max).filter(_.isSolved)
    val solution = if (solutions.hasNext) Some(solutions.next()) else None
    (solution, i.isEmpty)
  }

  /**
   * A solution of this puzzle. If there are multiple solutions one will be returned arbitrarily.
   */
  lazy val solution: Option[Grid] = solutions.headOption

  /**
   * All the solutions for this puzzle.
   */
  // TODO Should solutions be a stream? There aren't a lot of solutions, so we might as well cache them.
  lazy val solutions: TraversableView[Grid, Traversable[_]] = search.filter(_.isSolved)

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
 * Solver that uses the [[kenken.PermutationSetConstraint]] and [[kenken.UniquenessConstraint]] heuristics and sorts
 * guess cells by cage ambiguity.
 */
case class HeuristicSolver2(puzzle: Puzzle, hint: Option[Grid] = None) extends HeuristicSolver(puzzle, hint) {
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
case class HeuristicSolver1(puzzle: Puzzle, hint: Option[Grid] = None) extends HeuristicSolver(puzzle, hint)

abstract class HeuristicSolver(puzzle: Puzzle, hint: Option[Grid]) extends Solver(puzzle, hint) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints(puzzle.n,
        (cells => Seq(PermutationSetConstraint(puzzle.n, cells), UniquenessConstraint(cells)))))
}

object Solver {
  private val defaultAlgorithm = HeuristicSolver2

  /**
   * Solutions of a puzzle using the default solving algorithm
   * @param puzzle a puzzle
   * @param hint a grid to start from, or a maximally ambiguous grid if `None` is specified
   * @return the puzzle's solutions
   */
  def solutions(puzzle: Puzzle, hint: Option[Grid] = None): TraversableView[Grid, Traversable[_]] =
    defaultAlgorithm(puzzle, hint).solutions

  /**
   * Find all the puzzle solutions with the default algorithm, abandoning the search after a maximum number of partial
   * solutions.
   * @param puzzle a puzzle
   * @param max the maximum number of partial iterations
   * @param hint a grid to start from, or a maximally ambiguous grid if `None` is specified
   * @return tuple (solutions, `true` if the entire space was searched)
   */
  def cappedSolutions(puzzle: Puzzle, max: Int, hint: Option[Grid] = None): (List[Grid], Boolean) =
    defaultAlgorithm(puzzle, hint).cappedSolutions(max)

  /**
   * Solve all the puzzles in a file. Validate the answers if a '-v' switch is provided. Only search a specified number
   * of partial solutions if a '-m' switch is provided. Find all the solutions if the '-a' switch is provided;
   * otherwise just return the first solution discovered.
   *
   * Treat # as a comment delimiter in the puzzle file and skip leading blank lines.
   */
  def main(args: Array[String]) {
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
          case s :: tail if (s(0) == '-') => {
            println("Invalid switch " + s)
            sys.exit(-1)
          }
          case arg :: tail => parseCommandLineRec(tail, arg :: positional, option)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      require(positional.size == 1, "Incorrect number of arguments")
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
        val (solutions, complete) = max match {
          case Some(m) => if (firstOnly) {
            val s = solver.cappedSolution(m)
            (s._1.toSeq, s._2)
          } else solver.cappedSolutions(m)
          case None => if (firstOnly) (solver.solution.toSeq, true) else (solver.solutions, true)
        }
        solutions.foreach {
          solution =>
            println(solution)
            println(if (validate)
              validator.get.isPossibleSolution(solution) match {
                case true => "VALID\n"
                case false => "INVALID\n"
              }
            else "")
        }
        println(if (!complete) "INCOMPLETE\n" else "")
    }
  }
}