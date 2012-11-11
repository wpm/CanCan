package kenken

object Misc {
  // Long Time to even get to first solution
  val p = KenKen( """a=2880x b=6+ c=720x d=21+ e=1 f=3 g=2 h=6 i=36x j=4 k=144x l=5 m=5
                    |a a b b c c
                    |a a a a c c
                    |d d d e c f
                    |d d d g c h
                    |i i j k k l
                    |i i m k k k""".stripMargin)
  val s = Grid( """4 6 1 5 3 2
                  |5 3 2 4 6 1
                  |2 4 6 1 5 3
                  |1 5 3 2 4 6
                  |3 2 4 6 1 5
                  |6 1 5 3 2 4""".stripMargin)
}
