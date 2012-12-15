package cancan

import collection.TraversableView

/**
 * A strategy for enumerating all the possible solutions of a puzzle.
 *
 * A brute force solution would simply try all possible solutions for a grid.
 * This can be implemented as a search of a directed graph of partial solution grids where the edges point from partial
 * solutions to partial solutions containing where a candidate number has been removed from one of the cells.
 * Vertices with no outgoing edges are possible solutions.
 *
 * A depth-first search of the graph starting from a completely unsolved grid is infeasible.
 * However, at each guessed solution vertex, CanCan applies all the constraints, then all the constraints that apply to
 * any modified cells, and so on recursively.  This process is called ''constraint propagation'', and eliminates
 * possible values from some grids while revealing others as inconsistent with the constraints.
 * At each node it reduces the size of the search space to the point where an exhaustive search of the constrained
 * graph is tractable.
 *
 * The basic algorithm may be customized by specifying a [[cancan.ConstraintStrategy]] and a search strategy. The former
 * specifies a set of constraints to propagate. The latter specifies the rule by which to choose a cell to guess a value
 * for.
 *
 * @param strategyFactory function that creates a [[cancan.ConstraintStrategy]] from a [[cancan.Puzzle]]
 */
abstract class SearchStrategy(strategyFactory: (Puzzle => ConstraintStrategy))
  extends (Puzzle => TraversableView[Grid, Traversable[_]]) {

  /**
   * Lazily enumerate all possible solutions to a puzzle.
   *
   * @param puzzle the puzzle to solve
   * @return view of all possible solutions to a puzzle
   */
  def apply(puzzle: Puzzle): TraversableView[Grid, Traversable[_]] = {
    def search(grid: Grid, strategy: ConstraintStrategy): TraversableView[Grid, Traversable[_]] = {
      def nextGrids(grid: Grid): TraversableView[Grid, Traversable[_]] = {
        for {cell <- guessCell(grid, puzzle).toTraversable.view
             value <- guessValue(grid, cell).par
             next <- strategy(grid + (cell -> Set(value)), strategy.constraintMap(cell))}
        yield next
      }

      Traversable(grid).view ++ nextGrids(grid).flatMap(g => search(g, strategy))
    }

    var strategy = strategyFactory(puzzle)
    strategy(Grid(puzzle.n), puzzle.cageConstraints) match {
      case Some(grid) => search(grid, strategy)
      case None => Traversable[Grid]().view
    }
  }

  protected def guessCell(grid: Grid, puzzle: Puzzle): Option[Cell] = {
    val u = grid.unsolved
    if (u.isEmpty) None
    else Some(u.toSeq.map {
      case (cell, values) => (values.size, cell)
    }.min._2)
  }

  protected def guessValue(grid: Grid, cell: Cell): Seq[Int] = grid(cell).toSeq.sorted
}

/**
 * Make a guess in a cell that has the fewest possible values.
 */
case class OrderByCellSize(constraintStrategy: (Puzzle => ConstraintStrategy) = PreemptiveSet(_))
  extends SearchStrategy(constraintStrategy) {
}

/**
 * Make a guess in a cell that has the fewest possible values, then by cage ambiguity.
 */
case class OrderByCellThenCage(constraintStrategy: (Puzzle => ConstraintStrategy) = PreemptiveSet(_))
  extends SearchStrategy(constraintStrategy) {
  override protected def guessCell(grid: Grid, puzzle: Puzzle): Option[Cell] = {
    val u = grid.unsolved
    if (u.isEmpty) None
    else {
      val cageSize = cageAmbiguity(grid, puzzle)
      Some(u.toSeq.map {
        case (cell, values) =>
          (values.size, cageSize(puzzle.containingCages.get(cell)), cell)
      }.min._3)
    }
  }

  private def cageAmbiguity(grid: Grid, puzzle: Puzzle): Map[Option[Constraint], Int] = {
    Map[Option[Constraint], Int]().withDefaultValue(0) ++
      puzzle.containingCages.values.map(cage => Some(cage) -> (1 /: cage.cells.map(grid(_).size))(_ * _))
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
case class OracleSolver(solution: Grid, strategyFactory: (Puzzle => ConstraintStrategy) = PreemptiveSet(_))
  extends SearchStrategy(strategyFactory) {
  require(solution.isSolved, "Oracle solution is ambiguous\n" + solution)

  override protected def guessValue(grid: Grid, cell: Cell): Seq[Int] = solution(cell).toSeq

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
