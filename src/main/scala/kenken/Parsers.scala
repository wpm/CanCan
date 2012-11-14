package kenken

import util.parsing.combinator.{RegexParsers, JavaTokenParsers}
import util.parsing.input.Reader

/**
 * Parsers for string representations of grid and puzzle objects.
 */
object Parsers {
  private val eol = sys.props("line.separator")

  private object GridParser extends RegexParsers {
    override val whiteSpace = """[ \t]+""".r

    // 123 123 12
    //  1  23  123
    // 23  123 123
    //
    def grid: Parser[Grid] = repsep(row, eol) ^^ (rows => Grid(rows.map(_.toArray).toArray))

    // 123 123 12
    def row: Parser[List[Set[Int]]] = rep(cell)

    // 123
    def cell: Parser[Set[Int]] = """\d+""".r ^^ (s => Set[Int](s.toList.map(_.toString.toInt): _*))
  }

  private object PuzzleParser extends JavaTokenParsers {
    override val whiteSpace = """[ \t]+""".r

    private val opRegex = """(\d+)([+-x/])?""".r

    // a=5 b=7+ c=3x d=2-
    // a a b
    // a b b
    // c c d
    //
    def puzzle: Parser[KenKen] = cages ~ table ^^ {
      case (c ~ t) =>
        // The dimension of the grid is equal to the largest cell coordinate.
        val n = t.values.flatten.flatMap(cell => List(cell._1, cell._2)).max
        val cageConstraints = c.map {
          case (label, (value, op)) =>
            require(t.contains(label), "Table missing cage for " + label + " " + value + "" + op)
            (op, t(label)) match {
              case ("+", cells) => PlusConstraint(value, cells)
              case ("-", Vector(cell1, cell2)) => MinusConstraint(value, cell1, cell2)
              case ("x", cells) => TimesConstraint(value, cells)
              case ("/", Vector(cell1, cell2)) => DivideConstraint(value, cell1, cell2)
              case (null, Vector(cell)) => SpecifiedConstraint(value, cell)
              case _ => throw new IllegalArgumentException("Invalid cage " + label)
            }
        }
        KenKen(n, Set() ++ cageConstraints)
    }

    // a=5 b=7+ c=3x d=2-
    def cages: Parser[Map[String, (Int, String)]] = rep(cage) <~ eol ^^ (Map() ++ _)

    // b=7+
    def cage: Parser[(String, (Int, String))] = ident ~ "=" ~ op ^^ {
      case a ~ "=" ~ b => (a, b)
    }

    // 7+
    def op: Parser[(Int, String)] = opRegex ^^ {
      s: String =>
        val opRegex(value, operation) = s
        (value.toInt, operation)
    }

    // a a b
    // a b b
    // c c d
    //
    def table: Parser[Map[String, Vector[(Int, Int)]]] = repsep(tableRow, eol) ^^ {
      labels: List[List[String]] =>
        val n = labels.length
        require(labels.forall(_.length == n), "The table is not square:\n" + labels)
        val labeledCells = for ((row, r) <- labels.zipWithIndex;
                                (label, c) <- row.zipWithIndex)
        yield (label, (r + 1, c + 1))
        (Map[String, Vector[(Int, Int)]]() /: labeledCells) {
          case (m, (label, cell)) => m + (label -> (cell +: m.getOrElse(label, Vector())))
        }
    }

    // a a b
    def tableRow: Parser[List[String]] = rep(ident)

    implicit def parsePuzzleString(s: String) = parseAll(puzzle, s)

    implicit def parsePuzzleFile(r: Reader[Char]) = parseAll(puzzle, r)
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
  def parsePuzzle[T](implicit r: PuzzleParser.ParseResult[T]) = r match {
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
