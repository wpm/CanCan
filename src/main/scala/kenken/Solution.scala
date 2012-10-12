package kenken

/**
 * A solution to a KenKen problem.
 *
 * A solution is an assignment of values to cells. Solutions may be partial or complete. Solutions are always valid.
 * @param values array of values in the solution
 */
case class Solution private (values: Vector[Vector[Option[Int]]]) {
  def this(n:Int) = this(Vector.fill[Option[Int]](n,n)(null))

  def addHypothesis(h:Map[(Int, Int), Int]) = {
    val a = values.toArray.map(_.toArray)
    for (cell <- h.keys) a(cell._1)(cell._2) = Some(h(cell))
    Solution(Vector(a:_*).map(Vector(_:_*)))
  }

  def isComplete = !values.flatten.contains(null)

  def apply(cell:(Int, Int)) = values(cell._1)(cell._2)

  override def toString = values.map(_.map(v=>if (v == null) "." else v.get).mkString(" ")).mkString("\n")
}

object Solution {
  def apply(n:Int) = new Solution(n)
}
