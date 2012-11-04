package kenken

object Test {
  val p1 = Puzzle(2, List(SpecifiedConstraint(1, (1, 1))))

  //    4 3 1 2
  //    3 1 2 4
  //    2 4 3 1
  //    1 2 4 3
  val p2 = Puzzle( """a b b b
                     |a c d d
                     |e c d f
                     |e e f f
                     |a 1-
                     |b 6+
                     |c 5+
                     |d 9+
                     |e 5+
                     |f 8+""".stripMargin)

  //    2 1 4 3
  //    4 3 2 1
  //    1 4 3 2
  //    3 2 1 4
  val p3 = Puzzle( """a a b b
                     |c d d e
                     |c f f g
                     |c h h g
                     |a 2/
                     |b 1-
                     |c 12x
                     |d 1-
                     |e 1
                     |f 12x
                     |g 2/
                     |h 3+""".stripMargin)


  val p4 = Puzzle(
    """a a a b
      |c a d b
      |c e d d
      |c e f f
      |a 12+
      |b 3+
      |c 7+
      |d 9+
      |e 5+
      |f 4+""".stripMargin)


  def main(args: Array[String]) {
    println(p1.solve.toList)

    println(p2)
    println(p2.solve.toList)

    println(p3)
    println(p3.solve.toList)
  }
}
