package kenken

import scala._
import annotation.tailrec
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
  private val constraintMap: Map[(Int, Int), Set[Constraint]] =
    (Map[(Int, Int), Set[Constraint]]() /:
      (for {constraint <- cageConstraints ++ Constraint.latinSquareConstraints(n)
            cell <- constraint.cells}
      yield (cell -> constraint))) {
      case (m, (cell, constraint)) => m + (cell -> (m.getOrElse(cell, Set()) + constraint))
    }

  /**
   * A stream of all the grids that solve this puzzle.
   */
  lazy val solutions: Stream[Grid] = {
    applyConstraints(Grid(n), cageConstraints) match {
      case Some(g) => search(g).toStream.distinct
      case None => Stream.empty[Grid]
    }
  }

  private def search(grid: Grid = Grid(n)): SeqView[Grid, Seq[_]] = {
    def leastAmbiguousCell = {
      def sizeThenPosition(a: ((Int, Int), Set[Int]), b: ((Int, Int), Set[Int])): Boolean =
        Ordering[(Int, (Int, Int))].compare((a._2.size, a._1), (b._2.size, b._1)) < 0
      grid.unsolved.sortWith(sizeThenPosition).head
    }

    def guessCellValues(values: Set[Int]) = values.map(values - _).toSeq.view

    def applyGuessToGrid(cell: (Int, Int), guess: Set[Int]) =
      applyConstraints(grid + (cell -> guess), constraintMap(cell)).toSeq

    if (grid.isSolved)
      Vector(grid).view
    else {
      val (cell, values) = leastAmbiguousCell
      for {guess <- guessCellValues(values)
           newGrid <- applyGuessToGrid(cell, guess)
           solution <- search(newGrid)
      } yield solution
    }
  }

  /**
   * Return grid with the specified constraints applied. Return `None` if the grid is inconsistent with the
   * constraints.
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

  /**
   * This is the inverse of `KenKen(s)`
   */
  override def toString = {
    // Map of cells to the cages that contain them.
    val cageMap = Map() ++ constraintMap.map {
      case (cell, constraints) =>
        val cageConstraints = constraints.filter(_.isInstanceOf[CageConstraint])
        require(cageConstraints.size <= 1, "More than one cage constraint for " + cell + "\n" + cageConstraints)
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

  /**
   * String representation of the puzzle in the format recognized by the [[http://www.mlsite.net/neknek NekNek solver]].
   */
  def toNekNekString: String = {
    require(n < 10, "The largest puzzle Nek Nek handles is 9x9")
    "#\t" + n + "\n" + constraintMap.values.flatten.
      filter(_.isInstanceOf[CageConstraint]).toList.distinct.map(_.asInstanceOf[CageConstraint].toNekNekString).
      mkString("\n")
  }
}

object KenKen {
  def apply(n: Int, cageConstraints: Set[Constraint] = Set()): KenKen = new KenKen(n, cageConstraints)

  def apply(s: String): KenKen = Parsers.parsePuzzle(s)

  def main(args: Array[String]) {
    val in: FileReader = new FileReader(args(0))
    val puzzle = Parsers.parsePuzzle(in)
    println(puzzle.solutions.mkString("\n"))
  }
}
