package com.github.wpm.cancan

import annotation.tailrec

/**
 * Set of constraints for a puzzle that can be applied to a markup.
 */
abstract class ConstraintSet(puzzle: Puzzle) {
  /**
   * Map of cells in the puzzle markup to the constraints that contain them
   */
  val cellMap: Map[Cell, Set[Constraint]]

  /**
   * All the constraints in this set
   */
  def constraints: Set[Constraint] = cellMap.values.reduce(_ ++ _)

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
            case (cell, _) => cellMap(cell)
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
  override val cellMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints((cells => Seq(AllDifferentConstraint(cells)))))
}

/**
 * Implementation of the row and column constraints with the [[com.github.wpm.cancan.PreemptiveSetConstraint]] and
 * [[com.github.wpm.cancan.UniquenessConstraint]] heuristics.
 *
 * This is faster than the [[com.github.wpm.cancan.LatinSquare]] constraint strategy.
 *
 * @param puzzle puzzle to which to apply the constraints
 */
case class PreemptiveSet(puzzle: Puzzle) extends ConstraintSet(puzzle) {
  override val cellMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints((cells => Seq(
        AllDifferentConstraint(cells),
        PreemptiveSetConstraint(cells),
        UniquenessConstraint(cells)
      ))))
}