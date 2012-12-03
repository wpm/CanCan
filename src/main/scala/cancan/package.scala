/**
 * CanCan is a solver and generator of [[http://www.kenken.com KenKen]] puzzles.
 */
package object cancan {
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
   * @param strategy a search strategy, by default [[cancan.OrderByCellSize]]
   * @return the puzzle's solutions
   */
  def solutions(implicit puzzle: Puzzle,
                hint: Option[Grid] = None,
                strategy: SearchStrategy = OrderByCellSize()): Stream[Grid] =
    strategy(puzzle, hint).filter(_.isSolved).toStream

  /**
   * All the solutions of the puzzle up to `max` partial solution states.
   *
   * This is used to abandon difficult to solve puzzles after a finite amount of time.
   *
   * @param max the maximum number of partial solutions to search
   * @param strategy a search strategy, by default [[cancan.OrderByCellSize]]
   * @return tuple (solutions, `true` if all solutions have been searched)
   */
  def cappedSolutions(implicit puzzle: Puzzle,
                      max: Int,
                      hint: Option[Grid] = None,
                      strategy: SearchStrategy = OrderByCellSize()): (Stream[Grid], Boolean) = {
    val partialSolutions = strategy(puzzle, hint)
    (partialSolutions.take(max).filter(_.isSolved).toStream, partialSolutions.drop(max).isEmpty)
  }

  implicit def stringToPuzzle(s: String): Puzzle = Puzzle(s)
}
