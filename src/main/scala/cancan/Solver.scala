package cancan

import annotation.tailrec

/**
 * Program for solving KenKen puzzles.
 */
object Solver {
  private val usage =
    """solve [-a|-m|-v] file
      |
      |Solve the puzzles in a file.
      |
      |    -a - find all solutions instead of just the first one
      |    -m - maximum number of partial solutions to search before giving up
      |    -v - validate the returned solutions""".stripMargin

  /**
   * Solve all the puzzles in a file.
   */
  def solve(args: Array[String]) {
    def parseCommandLine(args: Array[String]): (String, Boolean, Boolean, Option[Int]) = {
      @tailrec
      def parseCommandLineRec(args: List[String],
                              positional: List[String],
                              option: Map[Symbol, String]): (List[String], Map[Symbol, String]) = {
        args match {
          case Nil => (positional.reverse, option)
          case "-a" :: tail => parseCommandLineRec(tail, positional, option + ('all -> ""))
          case "-m" :: m :: tail if (m.matches( """\d+""")) =>
            parseCommandLineRec(tail, positional, option + ('max -> m))
          case "-v" :: tail => parseCommandLineRec(tail, positional, option + ('validate -> ""))
          case "-h" :: tail => Dispatcher.error(usage)
          case s :: tail if (s(0) == '-') => Dispatcher.error("Invalid switch " + s)
          case arg :: tail => parseCommandLineRec(tail, arg :: positional, option)
        }
      }
      val (positional, option) = parseCommandLineRec(args.toList, Nil, Map())
      Dispatcher.errorIf(positional.size != 1, "Incorrect number of arguments")
      val max = option.get('max) match {
        case Some(s) => Some(s.toInt)
        case None => None
      }
      (positional.head, !option.contains('all), option.contains('validate), max)
    }

    val (filename, firstOnly, validate, max) = parseCommandLine(args)
    readPuzzlesFromFile(filename).zipWithIndex.foreach {
      case (puzzle, i) =>
        println((i + 1) + ".\n" + puzzle + "\n")
        val (ss, remaining) = max match {
          case Some(m) => cappedSolutions(puzzle, m)
          case None => (solutions(puzzle), false)
        }
        for (solution <- (if (firstOnly) ss.headOption.toStream else ss)) {
          println(solution)
          println(if (validate)
            puzzle.isPossibleSolution(solution) match {
              case true => "VALID\n"
              case false => "INVALID\n"
            }
          else "")
        }
        println(if (remaining) "...\n" else "")
    }
  }
}