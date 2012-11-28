package kenken

object Misc {
  val p1 = Puzzle( """a=4x b=12x c=15+ d=3/ e=11+ f=1- g=11+ h=1- i=9+ j=2- k=60x l=3- m=4x n=1- o=5-
                     |a a b b c c
                     |a d d e e c
                     |f g h h i j
                     |f g g i i j
                     |k k l l m m
                     |k n n o o m""".stripMargin)
  val s1 = HeuristicSolver1(p1)

  val p2 = Puzzle( """a=19+ b=12x c=1 d=22+ e=600x f=22+ g=3 h=16+ i=72x
                     |a b b c d d
                     |a a e d d d
                     |a a e f d g
                     |a e e f f f
                     |h e e i f f
                     |h h h i i i""".stripMargin)
  val s2 = HeuristicSolver1(p2)

  // Still slow
  val p3 = Puzzle( """a=315x b=1- c=24+ d=2- e=5 f=22+ g=25+ h=210x i=378x j=13+ k=22+ l=22+ m=144x n=18+ o=18+ p=24x q=5 r=22+ s=126x t=240x u=14+ v=24+ w=20+ x=6 y=1
                     |a a a b c c c d d
                     |e f a b g c h h i
                     |f f f g g j h h i
                     |k l l m g j j n i
                     |k l l m o o j n i
                     |k k m m o o p n q
                     |r r s s t t p p p
                     |r r s u t t v v w
                     |x y s u u v v w w""".stripMargin)
  //  1 7 5 2 9 3 8 4 6
  //  5 2 9 3 8 4 6 1 7
  //  9 3 8 4 6 1 7 5 2
  //  8 4 6 1 7 5 2 9 3
  //  3 8 4 6 1 7 5 2 9
  //  2 9 3 8 4 6 1 7 5
  //  7 5 2 9 3 8 4 6 1
  //  4 6 1 7 5 2 9 3 8
  //  6 1 7 5 2 9 3 8 4


  def main(args: Array[String]) {
    HeuristicSolver1(p3).solutions.foreach(println)
  }
}
