package kenken

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[kenken.KenKen]] object.
 */
class KenKenSpec extends FlatSpec {
  "A 2x2 Latin square" should "have two solutions" in {
    val s1 = Grid(
      """1 2
        |2 1""".stripMargin)
    val s2 = Grid(
      """2 1
        |1 2""".stripMargin)
    expect(Set(s1, s2)) {
      Set(KenKen(2).solutions.toList: _*)
    }
  }

  "Some example 4x4 KenKen puzzles" should "have solutions" in {
    expect(Option(Grid(
      """4 3 1 2
        |3 1 2 4
        |2 4 3 1
        |1 2 4 3""".stripMargin))) {
      KenKen( """a=1- b=6+ c=5+ d=9+ e=5+ f=8+
                |a b b b
                |a c d d
                |e c d f
                |e e f f""".stripMargin).solution
    }

    expect(Option(Grid(
      """2 1 4 3
        |4 3 2 1
        |1 4 3 2
        |3 2 1 4""".stripMargin))) {
      KenKen( """a=2/ b=1- c=12x d=1- e=1 f=12x g=2/ h=3+
                |a a b b
                |c d d e
                |c f f g
                |c h h g""".stripMargin).solution
    }

    expect(Option(Grid(
      """3 1 4 2
        |2 4 3 1
        |1 3 2 4
        |4 2 1 3""".stripMargin))) {
      KenKen( """a=12+ b=3+ c=7+ d=9+ e=5+ f=4+
                |a a a b
                |c a d b
                |c e d d
                |c e f f""".stripMargin).solution
    }
  }
}
