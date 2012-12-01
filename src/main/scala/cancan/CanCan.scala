package cancan

object CanCan {
  private val usage =
    """CanCan solve|generate|analyze
      |
      |    solve - solve puzzles
      |    generate - generate puzzles
      |    analyze - analyze puzzle solutions
      |
      |Follow the command with '-h' for more information.""".stripMargin

  def error(msg: String, status: Int = 0): Nothing = {
    println(msg)
    sys.exit(status)
  }

  def errorIf(condition: Boolean, msg: String, status: Int = 0): Null = {
    if (condition) error(msg, status) else null
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
