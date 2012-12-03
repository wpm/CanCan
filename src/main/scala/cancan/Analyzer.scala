package cancan

import annotation.tailrec

/**
 * Program that counts the number of explored partial solutions for a set of puzzles.
 */
object Analyzer {
  private val usage =
    """analyze [-l] file
      |
      |Count the number of partial solutions in a set of puzzles.
      |
      |    -l - use the Latin Square constraints without heuristics
      |    -c - use the order by cage strategy""".stripMargin

  def analyze(args: Array[String]) {
    def parseCommandLine(args: Array[String]): (String, SearchStrategy) = {
      @tailrec
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Map[Symbol, String]): (List[String], Map[Symbol, String]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-h" :: tail => CanCan.error(usage)
          case "-l" :: tail => parseCommandLineRec(tail, positional, option + ('latinSquare -> ""))
          case "-c" :: tail => parseCommandLineRec(tail, positional, option + ('cageOrder -> ""))
          case s :: tail if (s(0) == '-') => CanCan.error("Invalid switch " + s)
          case arg :: tail => parseCommandLineRec(tail, arg :: positional, option)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      CanCan.errorIf(positional.size != 1, "Invalid number of arguments")
      (positional.head, SearchStrategy.select(option.contains('cageOrder), option.contains('latinSquare)))
    }

    val (filename, searchStrategy) = parseCommandLine(args)
    val puzzles = readPuzzlesFromFile(filename)
    var first = 0.0
    var all = 0.0
    puzzles.zipWithIndex.foreach {
      case (puzzle, i) =>
        val partialSolutions = searchStrategy(puzzle).toSeq.zipWithIndex.filter(_._1.isSolved)
        val s = for ((partialSolutionIndex, solution) <- partialSolutions.map(_._2).zipWithIndex)
        yield (solution + 1, partialSolutionIndex + 1)
        first += s.head._2
        all += s.last._2
        println(((i + 1) :: s.map(t => t._1 + ":" + t._2).toList).mkString("\t"))
    }
    println("Mean steps to first solution " + first / puzzles.size)
    println("Mean steps to all solutions " + all / puzzles.size)
  }
}