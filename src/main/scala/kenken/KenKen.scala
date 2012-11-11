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

  /**
   * A solution to the puzzle.
   * @return puzzle solution or `None` if it cannot be solved
   */
  def solution: Option[Grid] = {
    val i = solutions().iterator
    if (i.hasNext) Some(i.next()) else None
  }

  // TODO What about grids we've already seen?
  /**
   * All solutions to the puzzle.
   * @return a lazy sequence of all the grids that solve this puzzle
   */
  def solutions(grid: Grid = Grid(n), visited: mutable.Set[Grid] = mutable.Set[Grid]()): SeqView[Grid, Seq[_]] = {
    require(grid.n == n, "Incorrect sized grid")

    def search(grid: Grid): SeqView[Grid, Seq[_]] = {
      // TODO Why can't I put nextGrids into the for loop below?
      def nextGrids(grid: Grid): SeqView[Grid, Seq[_]] = {
        for {(cell, values) <- grid.unsolved.sortWith(sizeThenPosition).view
             guess <- values
             newGrid <- applyConstraints(grid + (cell -> Set(guess)), constraintMap(cell))}
        yield newGrid
      }

      if (grid.isSolved)
        Vector(grid).view
      else for {newGrid <- nextGrids(grid)
                if (!visited.contains(newGrid))
                solution <- search(newGrid)
      }
      yield {
        visited += solution
        solution
      }
    }

    applyConstraints(grid, cageConstraints) match {
      case Some(g) => search(g)
      case None => Vector().view
    }
  }

  /**
   * Apply constraints to the grid
   */
  @tailrec
  private def applyConstraints(grid: Grid, constraints: Set[Constraint]): Option[Grid] = {
    if (constraints.isEmpty) Option(grid)
    else {
      val constraint = constraints.head
      grid.constrain(constraint) match {
        case Some((g, cells)) => applyConstraints(g, constraints ++ cells.flatMap(constraintMap(_)) - constraint)
        case None => None
      }
    }
  }

  private def sizeThenPosition(a: ((Int, Int), Set[Int]), b: ((Int, Int), Set[Int])): Boolean = {
    Ordering[(Int, (Int, Int))].compare((a._2.size, a._1), (b._2.size, b._1)) < 0
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
}

object KenKen {
  // TODO Make this argument a set.
  def apply(n: Int, cageConstraints: Traversable[Constraint] = Set()): KenKen = new KenKen(n, Set() ++ cageConstraints)

  def apply(s: String): KenKen = Parsers.parsePuzzle(s)

  def main(args: Array[String]) {
    val in: FileReader = new FileReader(args(0))
    val puzzle = Parsers.parsePuzzle(in)
    puzzle.solution match {
      case Some(solution) => println(solution)
      case None => println("The puzzle cannot be solved.")
    }
  }
}
