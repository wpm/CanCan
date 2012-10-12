package kenken

import scala.Array

/**
 * A solution to a KenKen problem.
 *
 * A solution is an assignment of values to cells. Solutions may be partial or complete. Solutions are always valid.
 * @param values array of values in the solution
 */
case class Solution private (values: Array[Array[Option[Int]]]) {
  def this(n:Int) = this(Array.ofDim[Option[Int]](n,n))

  def isComplete = !values.flatten.contains(null)

  def apply(cell:(Int, Int)) = values(cell._1)(cell._2)

  def update(cell:(Int, Int), value:Option[Int]) {values(cell._1)(cell._2) = value}

  def update(cell:(Int, Int), value:Int) {update(cell, Some(value))}

  override def toString = values.map(_.map(v=>if (v == null) "." else v.get).mkString(" ")).mkString("\n")
}

object Solution {
  def apply(n:Int) = new Solution(n)
}
