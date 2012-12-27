package com.github.wpm.cancan

import util.parsing.input.Reader
import scala.IllegalArgumentException

/**
 * A [[http://www.kenken.com KenKen]] puzzle.
 *
 * This is a set of cage constraints associated with a puzzle size.
 * @param n the puzzle size
 * @param cageConstraints cage constraints in the puzzle
 */
case class Puzzle(n: Int, cageConstraints: Set[CageConstraint] = Set()) {
  /**
   * Map of cells to the cages that contain them. Each cell is contained by at most one cage.
   */
  lazy val containingCages: Map[Cell, Constraint] = {
    Map() ++ Constraint.constraintMap(cageConstraints).map {
      case (cell, cages) =>
        require(cages.size == 1, "Multiple cages for cell " + cell)
        (cell -> cages.head)
    }
  }

  lazy private val validator = LatinSquare(this)

  /**
   * Does the specified markup satisfy all the constraints in this puzzle?
   *
   * This function is provided as a debugging utility to check that the solver is returning the correct answers.
   *
   * @param markup the markup to check
   * @return `true` if the markup satisfies the constrains, `false` if any constraints are violated
   */
  def isPossibleSolution(markup: Markup): Boolean = validator(markup) != None

  /**
   * Counts of cage sizes in the puzzle
   *
   * @return Map of cage size to the number of cages of that size
   */
  def cageSizes: Map[Int, Int] = (Map[Int, Int]().withDefaultValue(0) /: cageConstraints) {
    case (m, c) =>
      val s = c.size
      m + (s -> (m(s) + 1))
  }

  /**
   * Distribution of cage sizes in the puzzle
   *
   * @return Map of cage size to the proportion of cages of that size
   */
  def cageSizeDistribution: Map[Int, Double] =
    Map[Int, Double]() ++ cageSizes.map(kv => kv._1 -> kv._2 / cageConstraints.size.toDouble)

  /**
   * This is the inverse of `KenKen(s)`
   */
  override def toString = {
    def base26AlphabeticString(n: Int) = {
      def digits(n: Int, base: Int): Stream[Int] =
        (n % base) #:: (if (n / base > 0) digits(n / base, base) else Stream[Int]())
      digits(n, 26).map(digit => ('a'.toInt + digit).toChar).reverse.mkString("")
    }
    // Cages sorted by proximity to upper left hand corner of the puzzle.
    val cages = containingCages.values.toList.distinct.sortWith((a, b) => a.cells.head.compare(b.cells.head) < 0)
    // Map of cages to short alphabetic names.
    val cageToName = Map() ++
      cages.zipWithIndex.map {
        case (cage, i) => (cage, base26AlphabeticString(i))
      }
    // Map of cells to cage names.
    val cellToName = Map() ++ containingCages.map {
      case (cell, cage) => (cell, cageToName(cage))
    }
    // Table of cage names using '.' for cells not in cages.
    val table = for (r <- (1 to n)) yield {
      for (c <- (1 to n); cell = Cell(r, c)) yield cellToName.getOrElse(cell, ".")
    }
    // List of cage constraints followed by markup of letters representing cages.
    val cageKey = cageToName.map {
      case (cage, name) => name + "=" + cage
    }.toSeq.sorted.mkString(" ")
    (if (!cageKey.isEmpty) cageKey + "\n" else "") + matrixToString(table)
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
   * Parser of a string representation of a [[com.github.wpm.cancan.Puzzle]].
   */
  object PuzzleParser extends MultilineParser {
    val cellLabel = """\w+""".r
    val opRegex = """(\d+)([+-x/])?""".r

    // 7+
    def op: Parser[(Int, String)] = opRegex ^^ {
      s: String =>
        val opRegex(value, operation) = s
        (value.toInt, operation)
    }

    // b=7+
    def cage: Parser[(String, (Int, String))] = cellLabel ~ "=" ~ op ^^ {
      case a ~ "=" ~ b => (a, b)
    }

    // a=5 b=7+ c=3x d=2-
    def cages: Parser[Map[String, (Int, String)]] = rep1sep(cage, inLineWhitespace) <~ lineDelimiter ^^ (Map() ++ _)

    // a a b
    def row: Parser[List[String]] = rep1sep(cellLabel, inLineWhitespace) <~ lineDelimiter

    // a a b
    // a b b
    // c c d
    def matrix: Parser[Map[String, Seq[Cell]]] = rep(row) ^^ {
      labels: List[List[String]] =>
        val n = labels.length
        require(labels.forall(_.length == n), "The matrix is not square:\n" + labels)
        val labeledCells = for ((row, r) <- labels.zipWithIndex;
                                (label, c) <- row.zipWithIndex)
        yield (label, Cell(r + 1, c + 1))
        (Map[String, Seq[Cell]]() /: labeledCells) {
          case (m, (label, cell)) => m + (label -> (cell +: m.getOrElse(label, Nil)))
        }
    }

    // a=5x b=7+ c=3x d=2
    // a a b
    // a b b
    // c c d
    def puzzle: Parser[Puzzle] = cages ~ matrix ^^ {
      case (c ~ t) =>
        // The dimension of the markup is equal to the largest cell coordinate.
        val n = t.values.flatten.flatMap(cell => List(cell.row, cell.col)).max
        val cageConstraints = c.map {
          case (label, (value, op)) =>
            require(t.contains(label), "Table missing cage for " + label + " " + value + "" + op)
            (op, t(label)) match {
              case ("+", cells) => PlusConstraint(value, cells)
              case ("-", Seq(cell1, cell2)) => MinusConstraint(value, cell1, cell2)
              case ("x", cells) => TimesConstraint(value, cells)
              case ("/", Seq(cell1, cell2)) => DivideConstraint(value, cell1, cell2)
              case (null, Seq(cell)) => SpecifiedConstraint(value, cell)
              case _ => throw new IllegalArgumentException("Invalid cage " + label + "=" + value + "" + op)
            }
        }
        Puzzle(n, Set() ++ cageConstraints)
    }

    def puzzles: Parser[List[Puzzle]] = repsep(puzzle, rep1(eol)) <~ opt(rep(eol))

    implicit def parsePuzzlesString(s: String) = parseAll(puzzles, s)

    implicit def parsePuzzlesFile(r: Reader[Char]) = parseAll(puzzles, r)
  }

  /**
   * Create a puzzle from a string representation.
   *
   * This is the inverse of `toString`.
   * @param s string representation
   * @return KenKen puzzle
   */
  def apply(s: String): Puzzle = PuzzleParser.parseAll(PuzzleParser.puzzle, s) match {
    case PuzzleParser.Success(a, _) => a
    case e: PuzzleParser.Failure => throw new IllegalArgumentException(e.toString())
  }

  /**
   * Read a set of KenKen puzzles from a file or string
   */
  def parsePuzzles(implicit r: PuzzleParser.ParseResult[List[Puzzle]]) = r match {
    case PuzzleParser.Success(a, _) => a
    case e: PuzzleParser.Failure => throw new IllegalArgumentException(e.toString())
  }
}
