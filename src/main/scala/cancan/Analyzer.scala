package cancan

import annotation.tailrec

/**
 * Program that counts the number of explored partial solutions for a set of puzzles.
 */
object Analyzer {
  private val usage =
    """analyze file
      |
      |Count the number of partial solutions in a set of puzzles""".stripMargin

  def analyze(args: Array[String]) {
    def parseCommandLine(args: Array[String]): String = {
      @tailrec
      def parseCommandLineRec(args: List[String], positional: List[String]): List[String] = {
        args match {
          case Nil => positional.reverse
          case "-h" :: tail => CanCan.error(usage)
          case s :: tail if (s(0) == '-') => CanCan.error("Invalid switch " + s)
          case arg :: tail => parseCommandLineRec(tail, arg :: positional)
        }
      }
      val positional = parseCommandLineRec(args.toList, Nil)
      CanCan.errorIf(positional.size != 1, "Invalid number of arguments")
      positional.head
    }

    val filename = parseCommandLine(args)
    val puzzles = readPuzzlesFromFile(filename)
    var total = 0.0
    puzzles.zipWithIndex.foreach {
      case (puzzle, i) =>
        val partialSolutions = defaultAlgorithm(puzzle).partialSolutions.zipWithIndex.filter(_._1.isSolved)
        val s = for ((partialSolutionIndex, solution) <- partialSolutions.map(_._2).zipWithIndex)
        yield (solution + 1, partialSolutionIndex + 1)
        total += s.head._2
        println(((i + 1) :: s.map(x => x._1 + ":" + x._2).toList).mkString("\t"))
    }
    println("Mean steps to first solution " + total / puzzles.size)
  }
}