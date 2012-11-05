package kenken

import util.parsing.combinator._


object Arith {
  def parseIt(s: String) = {
    // TODO Can GridParser be an object?
    class GridParser extends RegexParsers {
      override val whiteSpace = """[ \t]+""".r
      val eol: Parser[Any] = sys.props("line.separator")
      val cell: Parser[Set[Int]] = """\d+""".r ^^ (s => Set[Int](s.toList.map(_.toString.toInt): _*))

      def row: Parser[List[Set[Int]]] = rep(cell)

      def table: Parser[List[List[Set[Int]]]] = rep(row ~ opt(eol) ^^ (_._1))
    }

    val p = new GridParser()
    p.parse(p.table, s) match {
      case p.Success(a, _) => a.map(_.toArray).toArray
      case e: p.Failure => throw new IllegalArgumentException(e.toString())
    }
  }
}