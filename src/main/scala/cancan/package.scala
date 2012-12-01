/**
 * CanCan is a solver and generator of [[http://www.kenken.com KenKen]] puzzles.
 */
package object cancan {
  val defaultAlgorithm = HeuristicSolver1

  /**
   * Read a set of puzzles from a file, treating # as a comment delimiter and skipping leading blank lines.
   *
   * See [[cancan.StringRepresentation.PuzzleParser]] for details of the puzzle representation format.
   * @param filename name of the file containing the puzzles
   * @return puzzles
   */
  def readPuzzlesFromFile(filename: String): List[Puzzle] =
    StringRepresentation.parsePuzzles(StringRepresentation.readFile(filename))

  /**
   * Solutions of a puzzle using the default solving algorithm
   * @param puzzle a puzzle
   * @param hint a grid to start from, or a maximally ambiguous grid if `None` is specified
   * @return the puzzle's solutions
   */
  def solutions(puzzle: Puzzle, hint: Option[Grid] = None): Stream[Grid] =
    defaultAlgorithm(puzzle, hint).solutions

  /**
   * All the solutions of the puzzle up to `max` partial solution states.
   *
   * This is used to abandon difficult to solve puzzles after a finite amount of time.
   *
   * @param max the maximum number of partial solutions to search
   * @return tuple (solutions, stream of partial solutions beyond the ones searched)
   */
  def cappedSolutions(puzzle: Puzzle, max: Int, hint: Option[Grid] = None): (Stream[Grid], Stream[Grid]) = {
    val partialSolutions = defaultAlgorithm(puzzle, hint).partialSolutions
    (partialSolutions.take(max).filter(_.isSolved), partialSolutions.drop(max))
  }
}
