package kenken

object Misc {
  val g = Grid(
    """124 124 124   3
      |24 234 234   1
      |12  34  34  24
      |3  12  12   4""".stripMargin
  )

  val c = DivideConstraint(2, (3, 4), (4, 4))

  def main(args: Array[String]) {
    println(g.constrain(c))

    val n = 3
    val p = Puzzle(n, List(SpecifiedConstraint(1, (1, 1))))
//    println(p.propagateConstraints(Grid(n)))

    val invalid = Puzzle(n, List(SpecifiedConstraint(1, (1, 1)), SpecifiedConstraint(1, (1, 2))))
//    println(invalid.propagateConstraints(Grid(n)))
  }
}
