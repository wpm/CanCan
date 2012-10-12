package kenken

/**
 * A KenKen problem is an NxN grid along with a set of box constraints.
 *
 * @param n the size of the problem grid
 * @param boxes box constraints
 */
case class Problem(n:Int, boxes:List[BoxConstraint])

