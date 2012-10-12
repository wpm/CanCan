package kenken

/**
 * A KenKen solving strategy is an iterator over solutions to a given problem.
 *
 * By contract, a strategy must yield all complete solutions. Strategies may also yield
 * partial solutions.
 *
 * @param problem a problem to solve
 */
abstract class Strategy(problem: Problem) extends Iterable[Solution]

