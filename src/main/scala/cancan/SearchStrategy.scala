package cancan

import collection.TraversableView

/**
 * A strategy for enumerating all the possible solutions of a puzzle.
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
             value <- grid(cell).toSeq.sorted.par
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
