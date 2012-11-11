package kenken

import scala._
import annotation.tailrec
import collection.mutable
import collection.SeqView
import scala.Predef._
import scala.Some
import java.io.FileReader


/**
 * A [[http://www.kenken.com KenKen]] puzzle.
 *
 * This is a set of constraints on a square grid of numbers
 * @param n the dimension of the grid
 * @param cageConstraints cage constraints in the puzzle
 */
class KenKen(n: Int, cageConstraints: Set[Constraint] = Set()) {
  /**
   * Map of cells in the puzzle grid to the constraints that contain them
   */
  val constraintMap: Map[(Int, Int), Set[Constraint]] =
    (Map[(Int, Int), Set[Constraint]]() /:
      (for {constraint <- cageConstraints ++ Constraint.latinSquareConstraints(n)
            cell <- constraint.cells}
      yield (cell -> constraint))) {
      case (m, (cell, constraint)) => m + (cell -> (m.getOrElse(cell, Set()) + constraint))
    }

  // TODO What about grids we've already seen?
  def findSolutions(grid: Grid): SeqView[Grid, Seq[_]] = {
    // TODO Why can't I put nextGrids into the for loop below?
    def nextGrids(grid: Grid) = {
      for {(cell, values) <- grid.unsolved.sortWith(KenKen.sizeThenPosition).view
           guess <- values
           newGrid <- applyConstraints(grid + (cell -> Set(guess)), constraintMap(cell))}
      yield newGrid
    }
    if (grid.isSolved)
      Vector(grid).view
    else for {g <- nextGrids(grid)
              solution <- findSolutions(g)}
    yield solution
  }

  /**
   * Apply constraints to the grid
   */
  @tailrec
  final def applyConstraints(grid: Grid, constraints: Set[Constraint] = Set() ++ cageConstraints): Option[Grid] = {
    if (constraints.isEmpty) Option(grid)
    else {
      val constraint = constraints.head
      grid.constrain(constraint) match {
        case Some((g, cells)) => applyConstraints(g, constraints ++ cells.flatMap(constraintMap(_)) - constraint)
        case None => None
      }
    }
  }

  /**
   * A solution to the puzzle.
   * @return puzzle solution or `None` if it cannot be solved
   */
  def solution: Option[Grid] = solutions.take(1).toList match {
    case Nil => None
    case ss => Option(ss.head)
  }

  /**
   * All solutions to the puzzle.
   * @return a lazy sequence of all the grids that solve this puzzle
   */
  def solutions: SeqView[Grid, Seq[_]] = {
    // The search may turn up multiple copies of the same solution, so ensure
    // that this function returns a unique list.
    val visited = mutable.Set[Grid]()

    def search(grid: Grid, constraints: Set[Constraint]): SeqView[Grid, Seq[_]] = {
      applyConstraints(grid, constraints) match {
        case None => Nil.view
        case Some(g) if (visited.contains(g)) => Nil.view
        case Some(g) if (g.isSolved) => {
          visited += g
          List(g).view
        }
        case Some(g: Grid) => unsolvedCells(g).flatMap {
          case (cell, values) =>
            values.view.flatMap(value => search(g + (cell -> Set(value)), constraintMap(cell)))
        }
      }
    }
    search(Grid(n), cageConstraints)
  }

  def guess(grid: Grid, cell: (Int, Int), value: Int): Option[Grid] = {
    applyConstraints(grid + (cell -> Set(value)), Set() ++ constraintMap(cell))
  }

  /**
   * Unsolved cells and their values in order of the number of possible values
   */
  def unsolvedCells(grid: Grid) = {
    grid.filter {
      case (cell, values) => values.size > 1
    }.toList.sortWith {
      (a, b) => a._2.size.compare(b._2.size) < 0
    }.view
  }

  /**
   * This is the inverse of `KenKen(s)`
   */
  override def toString = {
    // Map of cells to the cages that contain them.
    val cageMap = Map() ++ constraintMap.map {
      case (cell, constraints) =>
        val cageConstraints = constraints.filter(_.isInstanceOf[CageConstraint])
        require(cageConstraints.size <= 1, "More than one cage constraint for " + cell)
        cell -> cageConstraints
    }.filter {
      case (cell, constraints) => !constraints.isEmpty
    }.map {
      case (cell, constraints) => cell -> constraints.head
    }
    // Map of cages to representative characters.
    // TODO Handle more than 26 cages.
    val cageName = Map() ++
      cageMap.values.toList.distinct.sortBy(_.cells.head).zipWithIndex.map {
        case (cage, i) => cage -> ('a'.toInt + i).toChar.toString
      }
    // Write the cages line above a grid with representative characters.
    val cageNames: List[String] = if (cageName.isEmpty) Nil
    else cageName.map {
      case (cage: CageConstraint, name) => name + "=" + cage.cageName
    }.toList.sorted.mkString(" ") :: Nil
    val grid: List[String] = (for (r <- (1 to n)) yield {
      for (c <- (1 to n)) yield cageMap.get((r, c)) match {
        case None => "." // Write this if the cell is not in a cage.
        case Some(cage) => cageName(cage)
      }
    }.mkString(" ")).toList
    (cageNames ::: grid).mkString("\n")
  }

  //val constraints = constraintMap.values.flatten.toList.distinct

  //  def validate(g: Grid): Boolean = constraints.forall(g.constrain(_) != None)
  //
  //  def violated(g: Grid) = constraints.filter(g.constrain(_) == None)
}

object KenKen {
  // TODO Make this argument a set.
  def apply(n: Int, cageConstraints: Traversable[Constraint] = Set()): KenKen = new KenKen(n, Set() ++ cageConstraints)

  def apply(s: String): KenKen = Parsers.parsePuzzle(s)

  def sizeThenPosition(a: ((Int, Int), Set[Int]), b: ((Int, Int), Set[Int])): Boolean = {
    Ordering[(Int, (Int, Int))].compare((a._2.size, a._1), (b._2.size, b._1)) < 0
  }

  def main(args: Array[String]) {
    val in: FileReader = new FileReader(args(0))
    val puzzle = Parsers.parsePuzzle(in)
    puzzle.solution match {
      case Some(solution) => println(solution)
      case None => println("The puzzle cannot be solved.")
    }
  }
}
