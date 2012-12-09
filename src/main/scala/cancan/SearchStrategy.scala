package cancan

import collection.TraversableView

/**
 * A strategy for enumerating all the possible solutions of a puzzle.
 *
 * @param strategyFactory function that creates a [[cancan.ConstraintStrategy]] from a [[cancan.Puzzle]]
 */
abstract class SearchStrategy(strategyFactory: (Puzzle => ConstraintStrategy))
  extends ((Puzzle, Option[Grid]) => TraversableView[Grid, Traversable[_]]) {

  /**
   * Lazily enumerate all possible solutions to a puzzle.
   *
   * @param puzzle the puzzle to solve
   * @param hint a grid to start from, or a maximally ambiguous grid if `None` is specified
   * @return view of all possible solutions to a puzzle
   */
  def apply(puzzle: Puzzle, hint: Option[Grid] = None): TraversableView[Grid, Traversable[_]] = {
    def search(grid: Grid, strategy: ConstraintStrategy): TraversableView[Grid, Traversable[_]] = {
      def nextGrids(grid: Grid): TraversableView[Grid, Traversable[_]] = {
        for {cell <- guessCell(grid, puzzle).toTraversable.view
             value <- grid(cell).par
             next <- strategy(grid + (cell -> Set(value)), strategy.constraintMap(cell))}
        yield next
      }

      Traversable(grid).view ++ nextGrids(grid).flatMap(g => search(g, strategy))
    }

    var init = if (hint == None) Grid(puzzle.n) else hint.get
    var strategy = strategyFactory(puzzle)
    strategy(init, puzzle.cageConstraints) match {
      case Some(grid) => search(grid, strategy)
      case None => Traversable[Grid]().view
    }
  }

  protected def guessCell(grid: Grid, puzzle: Puzzle): Option[Cell]
}

/**
 * Make a guess in a cell that has the fewest possible values.
 */
case class OrderByCellSize(constraintStrategy: (Puzzle => ConstraintStrategy) = PreemptiveSet(_))
  extends SearchStrategy(constraintStrategy) {

  override protected def guessCell(grid: Grid, puzzle: Puzzle): Option[Cell] = {
    val u = grid.unsolved
    if (u.isEmpty) None
    else Some(u.toSeq.map {
      case (cell, values) => (values.size, cell)
    }.min._2)
  }
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

object SearchStrategy {
  def select(cageOrder: Boolean, latinSquare: Boolean): SearchStrategy = (cageOrder, latinSquare) match {
    case (true, true) => OrderByCellThenCage(LatinSquare(_))
    case (true, false) => OrderByCellThenCage(PermutationSet(_))
    case (false, true) => OrderByCellSize(LatinSquare(_))
    case (false, false) => OrderByCellSize(PermutationSet(_))
  }
}