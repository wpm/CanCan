package com.github.wpm.cancan

/**
 * Top level dispatcher for commands that can be run from a prompt.
 */
object Dispatcher {
  private val usage =
    """CanCan solve|generate|analyze
      |
      |    solve - solve puzzles
      |    generate - generate puzzles
      |    analyze - analyze puzzle solutions
      |
      |Follow the command with '-h' for more information.""".stripMargin

  /**
   * Print message and exit.
   *
   * @param message the message
   * @param status program exit code
   * @return this function does not return
   */
  def error(message: String, status: Int = 0): Nothing = {
    println(message)
    sys.exit(status)
  }

  /**
   * Conditionally print message and exit.
   *
   * @param condition condition on which to exit
   * @param message the message
   * @param status program exit code
   * @return `null`
   */
  def errorIf(condition: Boolean, message: String, status: Int = 0): Null = {
    if (condition) error(message, status) else null
  }

  def main(args: Array[String]) {
    errorIf(args.isEmpty, usage)
    args.head match {
      case "solve" => Solver.solve(args.tail)
      case "generate" => Generator.generate(args.tail)
      case "analyze" => Analyzer.analyze(args.tail)
      case "help" => error(usage)
      case command => error("Invalid command " + command + "\n" + usage)
    }
  }
}
