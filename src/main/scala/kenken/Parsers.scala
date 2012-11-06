package kenken

import util.parsing.combinator.{RegexParsers, JavaTokenParsers}

/**
 * Parsers for string representations of grid and puzzle objects.
 */
object Parsers {
  private val eol = sys.props("line.separator")

  object GridParser extends RegexParsers {
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

  object PuzzleParser extends JavaTokenParsers {
    override val whiteSpace = """[ \t]+""".r

    private val opRegex = """(\d+)([+-x/])?""".r

    // a=5 b=7+ c=3x d=2-
    // a a b
    // a b b
    // c c d
    //
    def puzzle: Parser[Puzzle] = cages ~ table ^^ {
      case (c ~ t) =>
        // The dimension of the grid is equal to the largest cell coordinate.
        val n = t.values.flatten.flatMap(cell => List(cell._1, cell._2)).max
        val cageConstraints = c.map {
          case (label, (value, op)) =>
            require(t.contains(label))
            (op, t(label)) match {
              case ("+", cells) => PlusConstraint(value, cells)
              case ("-", cell1 :: cell2 :: Nil) => MinusConstraint(value, cell1, cell2)
              case ("x", cells) => TimesConstraint(value, cells)
              case ("/", cell1 :: cell2 :: Nil) => DivideConstraint(value, cell1, cell2)
              case (null, cell :: Nil) => SpecifiedConstraint(value, cell)
              case _ => throw new IllegalArgumentException("Invalid cage " + label)
            }
        }.toList
        Puzzle(n, cageConstraints)
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
    def table: Parser[Map[String, List[(Int, Int)]]] = repsep(tableRow, eol) ^^ {
      labels: List[List[String]] =>
        val n = labels.length
        // The table must be square.
        require(labels.forall(_.length == n))
        val labeledCells = for ((row, r) <- labels.zipWithIndex;
                                (label, c) <- row.zipWithIndex)
        yield (label, (r + 1, c + 1))
        (Map[String, List[(Int, Int)]]() /: labeledCells) {
          case (m, (label, cell)) => m + (label -> (cell :: m.getOrElse(label, Nil)))
        }
    }

    // a a b
    def tableRow: Parser[List[String]] = rep(ident)
  }

  def parseGrid(s: String): Grid = GridParser.parseAll(GridParser.grid, s) match {
    case GridParser.Success(a, _) => a
    case e: GridParser.Failure => throw new IllegalArgumentException(e.toString())
  }

  def parsePuzzle(s: String): Puzzle = PuzzleParser.parseAll(PuzzleParser.puzzle, s) match {
    case PuzzleParser.Success(a, _) => a
    case e: PuzzleParser.Failure => throw new IllegalArgumentException(e.toString())
  }
}
