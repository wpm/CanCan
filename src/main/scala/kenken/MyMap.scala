package kenken

import collection.immutable.HashMap

//class MyMap(n: Int) extends HashMap[Int, String]((1 to n).map(x => (x, x.toString)): _*)

class MyMap extends HashMap[Int, String]

object MyMap {
  def apply() = new MyMap ++ List(1 -> "one", 2 -> "two")

  def main(args: Array[String]) {
    val m = MyMap
    println(m)
  }
}
