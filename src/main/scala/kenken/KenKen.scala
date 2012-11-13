package kenken

import collection.SeqView
import annotation.tailrec
import io.Source
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq
import scala.math.Ordering.Implicits._

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
   * A stream of all the grids that solve this puzzle.
   */
  lazy val solutions: Stream[Grid] = {
    applyConstraints(Grid(n), cageConstraints) match {
      case Some(g) => search(g).toStream.distinct
      case None => Stream.empty[Grid]
    }
  }

  private def search(grid: Grid = Grid(n)): SeqView[Grid, Seq[_]] = {
    def leastAmbiguousCell = grid.unsolved.map {
      case (cell, values) => (values.size, cell)
    }.min._2

    def guessCellValues(values: Set[Int]) = values.map(values - _).toSeq.view

    def applyGuessToGrid(cell: (Int, Int), guess: Set[Int]) =
      applyConstraints(grid + (cell -> guess), constraintMap(cell)).toSeq

    if (grid.isSolved)
      Vector(grid).view
    else {
      val cell = leastAmbiguousCell
      for {guess <- guessCellValues(grid(cell))
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
   * This is the inverse of `KenKen(s)`
   */
  override def toString = {
    def base26AlphabeticString(n: Int) = {
      def digits(n: Int, base: Int): Stream[Int] =
        (n % base) #:: (if (n / base > 0) digits(n / base, base) else Stream[Int]())
      digits(n, 26).map(digit => ('a'.toInt + digit).toChar).reverse.mkString("")
    }
    // Map of cells to the cages that contain them. Each cell is contained by at most one cage.
    val cellToCage = Map() ++ (for {(cell, cs) <- constraintMap.toSeq
                                    c <- cs
                                    if (c.isInstanceOf[CageConstraint])}
    yield (cell, c))
    // Cages sorted by proximity to upper left hand corner of the puzzle.
    val cages = cellToCage.values.toList.distinct.sortWith((a, b) => a.cells.head < b.cells.head)
    // Map of cages to short alphabetic names.
    val cageToName = Map() ++
      cages.zipWithIndex.map {
        case (cage, i) => (cage, base26AlphabeticString(i))
      }
    // Map of cells to cage names.
    val cellToName = Map() ++ cellToCage.map {
      case (cell, cage) => (cell, cageToName(cage))
    }
    // Table of cage names using '.' for cells not in cages.
    val table = for (r <- (1 to n)) yield {
      for (c <- (1 to n); cell = (r, c)) yield cellToName.getOrElse(cell, ".")
    }
    // List of cage constraints followed by grid of letters representing cages.
    val cageKey = cageToName.map {
      case (cage, name) => name + "=" + cage
    }.toSeq.sorted.mkString(" ")
    (if (!cageKey.isEmpty) cageKey + "\n" else "") + StringRepresentation.tableToString(table)
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

  def apply(s: String): KenKen = StringRepresentation.parsePuzzle(s)

  def main(args: Array[String]) {
    // Treat # as a comment delimiter.
    val lines = Source.fromFile(args(0)).getLines().map(_.replaceAll("#.*", ""))
    val in = new PagedSeqReader(PagedSeq.fromLines(lines))
    StringRepresentation.parsePuzzles(in).foreach {
      puzzle => println(puzzle + "\n\n" + puzzle.solutions.mkString("\n\n") + "\n\n")
    }
  }
}
