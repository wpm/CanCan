package cancan

import annotation.tailrec

/**
 * Set of constraints for a puzzle that can be applied to a markup.
 */
abstract class ConstraintSet(puzzle: Puzzle) {
  /**
   * Map of cells in the puzzle markup to the constraints that contain them
   */
  val constraintMap: Map[Cell, Set[Constraint]]

  /**
   * All the constraints in this set
   */
  def constraints: Set[Constraint] = constraintMap.values.reduce(_ ++ _)

  /**
   * Apply the specified constraints to a markup
   *
   * @param markup the markup to constrain
   * @param constraints the constraints to apply
   * @return a constrained markup or `None` if the markup is inconsistent with the constraints
   */
  @tailrec
  final def apply(markup: Markup, constraints: Set[_ <: Constraint] = constraints): Option[Markup] = {
    if (constraints.isEmpty) Option(markup)
    else {
      val constraint = constraints.head
      constraint(markup) match {
        case Some(changes) => {
          val triggeredConstraints = changes.flatMap {
            case (cell, _) => constraintMap(cell)
          }
          apply(markup ++ changes, constraints ++ triggeredConstraints - constraint)
        }
        case None => None
      }
    }
  }

  /**
   * Utility to create row and column constraints for all rows and columns in a puzzle
   *
   * @param constraints function that creates constraints for a given row or column
   * @return row and column constraints for the entire puzzle
   */
  protected def rowColumnConstraints(constraints: Seq[Cell] => Seq[Constraint]) = {
    val n = puzzle.n
    for {i <- (1 to n)
         cells <- Seq(Markup.row(n)(i), Markup.col(n)(i))
         constraint <- constraints(cells)
    } yield constraint
  }
}

/**
 * A set of Latin Square constraints and the cage constraints.
 *
 * This is the minimal set of constraints and employs no heuristics.
 *
 * @param puzzle puzzle to which to apply the constraints
 */
case class LatinSquare(puzzle: Puzzle) extends ConstraintSet(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints((cells => Seq(AllDifferentConstraint(cells)))))
}

/**
 * Implementation of the row and column constraints with the [[cancan.PreemptiveSet]] heuristic.
 *
 * This is faster than the [[cancan.LatinSquare]] constraint strategy.
 *
 * @param puzzle puzzle to which to apply the constraints
 */
case class PreemptiveSet(puzzle: Puzzle) extends ConstraintSet(puzzle) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints((cells => Seq(
        AllDifferentConstraint(cells),
        PreemptiveSetConstraint(cells),
        UniquenessConstraint(cells)
      ))))
}