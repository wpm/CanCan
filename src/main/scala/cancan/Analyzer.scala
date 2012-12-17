package cancan

import annotation.tailrec

/**
 * Program that counts the number of explored partial solutions for a set of puzzles.
 */
object Analyzer {
  /**
   * Analyze a set of puzzles
   */
  def analyze(args: Array[String]) {
    val usage =
      """analyze [-l|-c] file [file...]
        |
        |Count the number of partial solutions in a set of puzzles.
        |
        |    file - file containing puzzles
        |
        |    -l - use the Latin Square constraints without heuristics
        |    -c - use the order-by-cage search strategy""".stripMargin

    def parseCommandLine(args: Array[String]): (Seq[String], Solver) = {
      def select(cageOrder: Boolean, latinSquare: Boolean): Solver = (cageOrder, latinSquare) match {
        case (true, true) => OrderByCellThenCage(LatinSquare(_))
        case (true, false) => OrderByCellThenCage(PreemptiveSet(_))
        case (false, true) => OrderByCellSize(LatinSquare(_))
        case (false, false) => OrderByCellSize(PreemptiveSet(_))
      }

      @tailrec
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Map[Symbol, String]): (List[String], Map[Symbol, String]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-h" :: tail => Dispatcher.error(usage)
          case "-l" :: tail => parseCommandLineRec(tail, positional, option + ('latinSquare -> ""))
          case "-c" :: tail => parseCommandLineRec(tail, positional, option + ('cageOrder -> ""))
          case s :: tail if (s(0) == '-') => Dispatcher.error("Invalid switch " + s)
          case arg :: tail => parseCommandLineRec(tail, arg :: positional, option)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      Dispatcher.errorIf(positional.size < 1, "Invalid number of arguments")
      (positional, select(option.contains('cageOrder), option.contains('latinSquare)))
    }

    val (filenames, searchStrategy) = parseCommandLine(args)
    val puzzles = filenames.flatMap(readPuzzlesFromFile(_))
    var first = 0.0
    var all = 0.0
    puzzles.zipWithIndex.foreach {
      case (puzzle, i) =>
        val partialSolutions = searchStrategy(puzzle).toSeq.zipWithIndex.filter(_._1.isSolution)
        val s = for ((partialSolutionIndex, solution) <- partialSolutions.map(_._2).zipWithIndex)
        yield (solution + 1, partialSolutionIndex + 1)
        first += s.head._2
        all += s.last._2
        println(((i + 1) :: s.map(t => t._1 + ":" + t._2).toList).mkString("\t"))
    }
    println("Mean steps to first solution %.4f".format(first / puzzles.size))
    println("Mean steps to all solutions %.4f".format(all / puzzles.size))
  }
}