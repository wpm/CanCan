package cancan

import annotation.tailrec

/**
 * Set of constraints for a puzzle that can be applied to a grid.
 */
abstract class ConstraintStrategy(puzzle: Puzzle) {
  /**
   * Map of cells in the puzzle grid to the constraints that contain them
   */
  val constraintMap: Map[Cell, Set[Constraint]]

  /**
   * All the constraints in this set
   */
  def constraints: Set[Constraint] = constraintMap.values.reduce(_ ++ _)

  /**
   * Apply the specified constraints to a grid
   *
   * @param grid the grid to constrain
   * @param constraints the constraints to apply
   * @return a constrained grid or `None` if the grid is inconsistent with the constraints
   */
  @tailrec
  final def apply(grid: Grid, constraints: Set[_ <: Constraint] = constraints): Option[Grid] = {
    if (constraints.isEmpty) Option(grid)
    else {
      val constraint = constraints.head
      constraint(grid) match {
        case Some(changes) => {
          val triggeredConstraints = changes.flatMap {
            case (cell, _) => constraintMap(cell)
          }
          apply(grid ++ changes, constraints ++ triggeredConstraints - constraint)
        }
        case None => None
      }
    }
  }

  /**
   * Utility to create row and column constraints for all row and columns in a puzzle
   *
   * @param constraints function that creates constraints for a given row or column
   * @return row and column constraints for the entire puzzle
   */
  protected def rowColumnConstraints(constraints: Seq[Cell] => Seq[Constraint]) = {
    val n = puzzle.n
    for {i <- (1 to n)
         cells <- Seq(Grid.row(n)(i), Grid.col(n)(i))
         constraint <- constraints(cells)
    } yield constraint
  }
}

/**
 * A set of Latin Square constraints and the cage constraints.
 *
 * This is the minimal set of constraints and employs no heuristics.
 * @param puzzle puzzle to which to apply the strategy
 */
case class LatinSquare(puzzle: Puzzle) extends ConstraintStrategy(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints((cells => Seq(AllDifferentConstraint(cells)))))
}

/**
 * Implementation of the row and column constraints with the [[cancan.PermutationSet]] heuristic.
 *
 * This is faster than the [[cancan.LatinSquare]] strategy.
 * @param puzzle puzzle to which to apply the strategy
 */
case class PermutationSet(puzzle: Puzzle) extends ConstraintStrategy(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints((cells => Seq(PermutationSetConstraint(puzzle.n, cells), UniquenessConstraint(cells)))))
}

/**
 * Implementation of the row and column constraints with the [[cancan.PreemptiveSet]] heuristic.
 *
 * This is faster than the [[cancan.LatinSquare]] strategy.
 * @param puzzle puzzle to which to apply the strategy
 */
case class PreemptiveSet(puzzle: Puzzle) extends ConstraintStrategy(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints((cells => Seq(
        AllDifferentConstraint(cells),
        PreemptiveSetConstraint(cells),
        UniquenessConstraint(cells)
      ))))
}