package kenken

/**
 * Program that counts the number of explored partial solutions for a set of puzzles.
 */
object Counter {
  def main(args: Array[String]) {
    val filename = args(0)

    val puzzles = StringRepresentation.parsePuzzles(StringRepresentation.readFile(filename))
    var total = 0.0
    puzzles.zipWithIndex.foreach {
      case (puzzle, i) =>
        System.err.println((i + 1) + ".\n" + puzzle + "\n")
        val partialSolutions = HeuristicSolver2(puzzle).search.force
        val p = partialSolutions.size
        val s = partialSolutions.filter(_.isSolved).size
        println(p + "\t" + s)
        total += p
    }
    System.err.println("Mean partial solutions " + total / puzzles.size)
  }
}