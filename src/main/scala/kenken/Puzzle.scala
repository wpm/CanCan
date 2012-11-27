package kenken

/**
 * A [[http://www.kenken.com KenKen]] puzzle.
 *
 * This is a set of cage constraints associated with a puzzle size.
 * @param n the puzzle size
 * @param cageConstraints cage constraints in the puzzle
 */
case class Puzzle(n: Int, cageConstraints: Set[CageConstraint] = Set()) {
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
    val cellToCage = Map() ++ (for {(cell, cs) <- Constraint.constraintMap(cageConstraints).toSeq
                                    c <- cs}
    yield (cell, c))
    // Cages sorted by proximity to upper left hand corner of the puzzle.
    val cages = cellToCage.values.toList.distinct.sortWith((a, b) => a.cells.head.compare(b.cells.head) < 0)
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
      for (c <- (1 to n); cell = Cell(r, c)) yield cellToName.getOrElse(cell, ".")
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
    "#\t" + n + "\n" + cageConstraints.map(_.toNekNekString).mkString("\n")
  }
}

object Puzzle {
  /**
   * Create a puzzle from a string representation.
   *
   * This is the inverse of `toString`.
   * @param s string representation
   * @return KenKen puzzle
   */
  def apply(s: String): Puzzle = StringRepresentation.parsePuzzle(s)
}
