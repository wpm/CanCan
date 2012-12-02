package cancan

import collection.TraversableView
import annotation.tailrec

abstract class SearchTree(puzzle: Puzzle, hint: Option[Grid]) extends TraversableView[Grid, Traversable[_]] {
  /**
   * Map of cells in the puzzle grid to the constraints that contain them
   */
  val constraintMap: Map[Cell, Set[Constraint]]

  def foreach[U](f: (Grid) => U) {
    search(if (hint == None) Grid(puzzle.n) else hint.get).foreach(f)
  }

  /**
   * All the partial solutions of this puzzle, including the complete solutions.
   * @return all the partial solutions of this puzzle
   */
  private def search(init: Grid): TraversableView[Grid, Traversable[_]] = {
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

  protected def underlying = null
}

/**
 * Solver that just uses the Latin Square constraints and does not use any heuristics.
 */
case class LatinSquareSearchTree(puzzle: Puzzle, hint: Option[Grid] = None) extends SearchTree(puzzle, hint) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints(puzzle.n, (cells => Seq(LatinSquareConstraint(cells)))))
}

abstract class HeuristicSearchTree(puzzle: Puzzle, hint: Option[Grid]) extends SearchTree(puzzle, hint) {
  override val constraintMap =
    Constraint.constraintMap(puzzle.cageConstraints ++
      rowColumnConstraints(puzzle.n,
        (cells => Seq(PermutationSetConstraint(puzzle.n, cells), UniquenessConstraint(cells)))))
}

/**
 * Solver that uses the [[cancan.PermutationSetConstraint]] and [[cancan.UniquenessConstraint]] heuristics.
 */
case class PermutationSetSearchTree(puzzle: Puzzle, hint: Option[Grid] = None) extends HeuristicSearchTree(puzzle, hint)

/**
 * Solver that uses the [[cancan.PermutationSetConstraint]] and [[cancan.UniquenessConstraint]] heuristics and sorts
 * guess cells by cage ambiguity.
 */
case class PermutationSetByCageSizeSearchTree(puzzle: Puzzle,
                                              hint: Option[Grid] = None) extends HeuristicSearchTree(puzzle, hint) {
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

