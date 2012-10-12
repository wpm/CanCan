package kenken

/**
 * Given a problem and a partial solution, this object guesses the next solution.
 *
 * @param problem a problem to solve
 * @param current the current solution
 */
abstract class Guesser(problem: Problem, current: Solution) extends Iterator[Solution]

