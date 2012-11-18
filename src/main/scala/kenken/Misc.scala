package kenken

object Misc {
  val p1 = Puzzle( """a=4x b=12x c=15+ d=3/ e=11+ f=1- g=11+ h=1- i=9+ j=2- k=60x l=3- m=4x n=1- o=5-
                     |a a b b c c
                     |a d d e e c
                     |f g h h i j
                     |f g g i i j
                     |k k l l m m
                     |k n n o o m""".stripMargin)
  val s1 = FastSolver(p1)

  val p2 = Puzzle( """a=19+ b=12x c=1 d=22+ e=600x f=22+ g=3 h=16+ i=72x
                     |a b b c d d
                     |a a e d d d
                     |a a e f d g
                     |a e e f f f
                     |h e e i f f
                     |h h h i i i""".stripMargin)
  val s2 = FastSolver(p2)
}
