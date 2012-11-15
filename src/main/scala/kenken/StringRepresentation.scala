package kenken

import util.parsing.combinator.RegexParsers
import util.parsing.input.Reader

/**
 * String representations of grid and puzzle objects.
 */
object StringRepresentation {

  trait BaseParser extends RegexParsers {
    override val skipWhitespace = false
    val inLineWhitespace = """[ \t]+""".r
    val eol = sys.props("line.separator")
    val eoi = """\z""".r
    val lineDelimiter = (eol | eoi)
  }

  object GridParser extends BaseParser {
    // 12
    def cell: Parser[Set[Int]] = """\d+""".r ^^ (s => Set() ++ (s.toList.map(_.toString.toInt)))

    // 12 12
    def row: Parser[List[Set[Int]]] = opt(inLineWhitespace) ~> rep1sep(cell, inLineWhitespace) <~ lineDelimiter

    // 12 12
    // 12 12
    def grid: Parser[Grid] = rep(row) ^^ (Grid(_))

    // 12 12
    // 12 12
    //
    // 123 123 123
    // 123 123 123
    // 123 123 123
    def grids: Parser[List[Grid]] = repsep(grid, rep1(eol)) <~ opt(rep(eol))
  }

  object PuzzleParser extends BaseParser {
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
    def table: Parser[Map[String, Seq[(Int, Int)]]] = rep(row) ^^ {
      labels: List[List[String]] =>
        val n = labels.length
        require(labels.forall(_.length == n), "The table is not square:\n" + labels)
        val labeledCells = for ((row, r) <- labels.zipWithIndex;
                                (label, c) <- row.zipWithIndex)
        yield (label, (r + 1, c + 1))
        (Map[String, Seq[(Int, Int)]]() /: labeledCells) {
          case (m, (label, cell)) => m + (label -> (cell +: m.getOrElse(label, Nil)))
        }
    }

    // a=5x b=7+ c=3x d=2
    // a a b
    // a b b
    // c c d
    def puzzle: Parser[KenKen] = cages ~ table ^^ {
      case (c ~ t) =>
        // The dimension of the grid is equal to the largest cell coordinate.
        val n = t.values.flatten.flatMap(cell => List(cell._1, cell._2)).max
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
        KenKen(n, Set() ++ cageConstraints)
    }

    def puzzles: Parser[List[KenKen]] = repsep(puzzle, rep1(eol)) <~ opt(rep(eol))

    implicit def parsePuzzlesString(s: String) = parseAll(puzzles, s)

    implicit def parsePuzzlesFile(r: Reader[Char]) = parseAll(puzzles, r)
  }


  /**
   * Create a grid from a string representation.
   */
  def parseGrid(s: String): Grid = GridParser.parseAll(GridParser.grid, s) match {
    case GridParser.Success(a, _) => a
    case e: GridParser.Failure => throw new IllegalArgumentException(e.toString())
  }

  /**
   * Create a KenKen puzzle from a file or string.
   */
  def parsePuzzle(implicit r: PuzzleParser.ParseResult[List[KenKen]]) = parsePuzzles.head

  /**
   * Read a set of KenKen puzzles from a file or string
   */
  def parsePuzzles(implicit r: PuzzleParser.ParseResult[List[KenKen]]) = r match {
    case PuzzleParser.Success(a, _) => a
    case e: PuzzleParser.Failure => throw new IllegalArgumentException(e.toString())
  }

  /**
   * Print a 2-dimensional array of objects as a grid, centering each element.
   * @param table 2-dimensional array of objects
   * @tparam T object in the array
   * @return string representation
   */
  def tableToString[T](table: Traversable[Traversable[T]]) = {
    def centered(s: String, width: Int) = {
      val pad = (width - s.length) / 2
      ("%" + width + "s").format(" " * pad + s)
    }
    val widest = (for (row <- table; col <- row) yield col.toString.size).max
    table.map(row => row.map(col => centered(col.toString, widest)).mkString(" ")).mkString("\n")
  }
}
