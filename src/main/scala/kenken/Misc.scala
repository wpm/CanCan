package kenken

object Misc {
  // Long Time to even get to first solution
  val p6x6 = KenKen( """a=2880x b=6+ c=720x d=21+ e=1 f=3 g=2 h=6 i=36x j=4 k=144x l=5 m=5
                       |a a b b c c
                       |a a a a c c
                       |d d d e c f
                       |d d d g c h
                       |i i j k k l
                       |i i m k k k""".stripMargin)
  val s6x6 = Grid( """4 6 1 5 3 2
                     |5 3 2 4 6 1
                     |2 4 6 1 5 3
                     |1 5 3 2 4 6
                     |3 2 4 6 1 5
                     |6 1 5 3 2 4""".stripMargin)

  val p4x4 = KenKen( """a=2/ b=1- c=12x d=1- e=1 f=12x g=2/ h=3+
                       |a a b b
                       |c d d e
                       |c f f g
                       |c h h g""".stripMargin)

  val p1 = KenKen( """a=1- b=6+ c=5+ d=9+ e=5+ f=8+
                     |a b b b
                     |a c d d
                     |e c d f
                     |e e f f""".stripMargin)


  val p3 = KenKen( """a=12+ b=3+ c=7+ d=9+ e=5+ f=4+
                     |a a a b
                     |c a d b
                     |c e d d
                     |c e f f""".stripMargin)

  // NekNek solves it instantly.
  //  6 4 2 5 3 1
  //  3 1 6 4 2 5
  //  5 3 1 6 4 2
  //  1 6 4 2 5 3
  //  4 2 5 3 1 6
  //  2 5 3 1 6 4
  val unsolvable = KenKen( """a=24x b=13+ c=5 d=6x e=7+ f=30x g=10+ h=15+ i=12+ j=1- k=9+ l=24x m=3- n=6
                             |a a b c d d
                             |e e b b d f
                             |g e b h h f
                             |g i i j h f
                             |g i k j l l
                             |m m k k n l""".stripMargin)

  def main(args: Array[String]) {
    val puzzle = unsolvable
    println(puzzle)
    println(puzzle.solutions().toList)
  }
}
