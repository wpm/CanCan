package cancan

import collection.TraversableView
import annotation.tailrec

/**
 * An algorithm for solving a puzzle
 *
 * A brute force solution would simply try all possible solutions for a markup. This can be implemented as a search of a
 * directed graph of markups where the edges point from markups to markups with a guessed number in one of the cells.
 * Vertices with no outgoing edges are possible solutions.
 *
 * A depth-first search of the graph starting from a completely unsolved markup is infeasible.
 * However, at each guessed solution vertex, CanCan applies all the constraints, then all the constraints that apply to
 * any modified cells, and so on recursively.  This process is called ''constraint propagation'', and eliminates
 * possible values from some markups while revealing others as inconsistent with the constraints.
 * At each node it reduces the size of the search space to the point where an exhaustive search of the constrained
 * graph is tractable.
 *
 * The basic algorithm may be customized by specifying a [[cancan.ConstraintStrategy]] and a search strategy. The case
 * classes of [[cancan.Solver]] implement various search strategies. [[cancan.OrderByCellSize]] is the default. The
 * constraint strategy specifies a set of constraints to propagate. The search strategy specifies the rule by which to
 * choose guess a value for.
 *
 * @param strategyFactory function that creates a [[cancan.ConstraintStrategy]] from a [[cancan.Puzzle]]
 */
abstract class Solver(strategyFactory: (Puzzle => ConstraintStrategy))
  extends (Puzzle => TraversableView[Markup, Traversable[_]]) {

  /**
   * Lazily enumerate all possible solutions to a puzzle.
   *
   * @param puzzle the puzzle to solve
   * @return view of all possible solutions to a puzzle
   */
  def apply(puzzle: Puzzle): TraversableView[Markup, Traversable[_]] = {
    def search(markup: Markup, strategy: ConstraintStrategy): TraversableView[Markup, Traversable[_]] = {
      def nextMarkups(markup: Markup): TraversableView[Markup, Traversable[_]] = {
        for {cell <- guessCell(markup, puzzle).toTraversable.view
             value <- guessValue(markup, cell).par
             next <- strategy(markup + (cell -> Set(value)), strategy.constraintMap(cell))}
        yield next
      }

      Traversable(markup).view ++ nextMarkups(markup).flatMap(m => search(m, strategy))
    }

    val strategy = strategyFactory(puzzle)
    strategy(Markup(puzzle.n), puzzle.cageConstraints) match {
      case Some(markup) => search(markup, strategy)
      case None => Traversable[Markup]().view
    }
  }

  protected def guessCell(markup: Markup, puzzle: Puzzle): Option[Cell] = {
    val u = markup.unsolved
    if (u.isEmpty) None
    else Some(u.toSeq.map {
      case (cell, values) => (values.size, cell)
    }.min._2)
  }

  protected def guessValue(markup: Markup, cell: Cell): Seq[Int] = markup(cell).toSeq.sorted
}

/**
 * Make a guess in a cell that has the fewest possible values.
 */
case class OrderByCellSize(constraintStrategy: (Puzzle => ConstraintStrategy) = PreemptiveSet(_))
  extends Solver(constraintStrategy) {
}

/**
 * Make a guess in a cell that has the fewest possible values, then by cage ambiguity.
 */
case class OrderByCellThenCage(constraintStrategy: (Puzzle => ConstraintStrategy) = PreemptiveSet(_))
  extends Solver(constraintStrategy) {
  override protected def guessCell(markup: Markup, puzzle: Puzzle): Option[Cell] = {
    val u = markup.unsolved
    if (u.isEmpty) None
    else {
      val cageSize = cageAmbiguity(markup, puzzle)
      Some(u.toSeq.map {
        case (cell, values) =>
          (values.size, cageSize(puzzle.containingCages.get(cell)), cell)
      }.min._3)
    }
  }

  private def cageAmbiguity(markup: Markup, puzzle: Puzzle): Map[Option[Constraint], Int] = {
    Map[Option[Constraint], Int]().withDefaultValue(0) ++
      puzzle.containingCages.values.map(cage => Some(cage) -> (1 /: cage.cells.map(markup(_).size))(_ * _))
  }
}

/**
 * An oracle solver uses the puzzle answer to always guess the correct value when searching.
 *
 * The solution specified to the constructor must be a solution for any puzzle this solver attempts.
 *
 * @param solution solution to the puzzle that will be solved
 * @param strategyFactory function that creates a [[cancan.ConstraintStrategy]] from a [[cancan.Puzzle]]
 */
case class OracleSolver(solution: Markup, strategyFactory: (Puzzle => ConstraintStrategy) = PreemptiveSet(_))
  extends Solver(strategyFactory) {
  require(solution.isSolution, "Oracle solution is ambiguous\n" + solution)

  override protected def guessValue(markup: Markup, cell: Cell): Seq[Int] = solution(cell).toSeq

  /**
   * The difficulty of this puzzle
   *
   * This is defined to be the number of search steps required by an oracle solver
   *
   * @param puzzle puzzle to evaluate
   * @return puzzle difficulty
   */
  def difficulty(puzzle: Puzzle): Int = this(puzzle).size
}

/**
 * Program for solving KenKen puzzles.
 */
object Solver {
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
      @tailrec
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Map[Symbol, String]): (List[String], Map[Symbol, String]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-a" :: tail => parseCommandLineRec(tail, positional, option + ('all -> ""))
          case "-m" :: m :: tail if (m.matches( """\d+""")) =>
            parseCommandLineRec(tail, positional, option + ('max -> m))
          case "-v" :: tail => parseCommandLineRec(tail, positional, option + ('validate -> ""))
          case "-h" :: tail => Dispatcher.error(usage)
          case s :: tail if (s(0) == '-') => Dispatcher.error("Invalid switch " + s)
          case arg :: tail => parseCommandLineRec(tail, arg :: positional, option)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      Dispatcher.errorIf(positional.size != 1, "Incorrect number of arguments")
      val max = option.get('max) match {
        case Some(s) => Some(s.toInt)
        case None => None
      }
      (positional.head, !option.contains('all), option.contains('validate), max)
    }

    val (filename, firstOnly, validate, max) = parseCommandLine(args)
    readPuzzlesFromFile(filename).zipWithIndex.foreach {
      case (puzzle, i) =>
        println((i + 1) + ".\n" + puzzle + "\n")
        val (ss, remaining) = max match {
          case Some(m) => cappedSolutions(puzzle, m)
          case None => (solutions(puzzle), false)
        }
        for (solution <- (if (firstOnly) ss.headOption.toStream else ss)) {
          println(solution)
          println(if (validate)
            puzzle.isPossibleSolution(solution) match {
              case true => "VALID\n"
              case false => "INVALID\n"
            }
          else "")
        }
        println(if (remaining) "...\n" else "")
    }
  }
}
