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
      |    -l - use the Latin Square constraints without heuristics""".stripMargin

  def analyze(args: Array[String]) {
    def parseCommandLine(args: Array[String]): (String, Boolean) = {
      @tailrec
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Map[Symbol, String]): (List[String], Map[Symbol, String]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-h" :: tail => CanCan.error(usage)
          case "-l" :: tail => parseCommandLineRec(tail, positional, option + ('latinSquare -> ""))
          case s :: tail if (s(0) == '-') => CanCan.error("Invalid switch " + s)
          case arg :: tail => parseCommandLineRec(tail, arg :: positional, option)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      CanCan.errorIf(positional.size != 1, "Invalid number of arguments")
      (positional.head, option.contains('latinSquare))
    }

    val (filename, latinSquare) = parseCommandLine(args)
    val puzzles = readPuzzlesFromFile(filename)
    var total = 0.0
    val solver = if (latinSquare) LatinSquareSolver(_: Puzzle, None) else defaultAlgorithm(_: Puzzle, None)
    puzzles.zipWithIndex.foreach {
      case (puzzle, i) =>
        val partialSolutions = solver(puzzle).partialSolutions.zipWithIndex.filter(_._1.isSolved)
        val s = for ((partialSolutionIndex, solution) <- partialSolutions.map(_._2).zipWithIndex)
        yield (solution + 1, partialSolutionIndex + 1)
        total += s.head._2
        println(((i + 1) :: s.map(x => x._1 + ":" + x._2).toList).mkString("\t"))
    }
    println("Mean steps to first solution " + total / puzzles.size)
  }
}