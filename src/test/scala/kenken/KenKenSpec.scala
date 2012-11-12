package kenken

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[kenken.KenKen]] object.
 */
class KenKenSpec extends FlatSpec {

  val (ls_2x2_puzzle, ls_2x2_sol1, ls_2x2_sol2) = (
    """. .
      |. .""".stripMargin,
    """1 2
      |2 1""".stripMargin,
    """2 1
      |1 2""".stripMargin
    )

  behavior of "A 2x2 Latin square"

  it should "have solutions\n" + ls_2x2_sol1 + "\n" + ls_2x2_sol2 in {
    expect(Set(Grid(ls_2x2_sol1), Grid(ls_2x2_sol2))) {
      Set() ++ KenKen(2).solutions
    }
  }

  it should "print as\n" + ls_2x2_puzzle in {
    expect(ls_2x2_puzzle) {
      KenKen(2).toString
    }
  }

  val (kenken1_4x4_puzzle, kenken1_4x4_solution) = (
    """a=1- b=6+ c=5+ d=9+ e=5+ f=8+
      |a b b b
      |a c d d
      |e c d f
      |e e f f""".stripMargin,
    """4 3 1 2
      |3 1 2 4
      |2 4 3 1
      |1 2 4 3""".stripMargin
    )

  behavior of kenken1_4x4_puzzle

  it should "have solution\n" + kenken1_4x4_solution in {
    expect(List(Grid(kenken1_4x4_solution))) {
      KenKen(kenken1_4x4_puzzle).solutions
    }
  }

  it should "print as\n" + kenken1_4x4_puzzle in {
    expect(kenken1_4x4_puzzle) {
      KenKen(kenken1_4x4_puzzle).toString
    }
  }


  val (kenken2_24x4_puzzle, kenken2_4x4_solution) = (
    """a=2/ b=1- c=12x d=1- e=1 f=12x g=2/ h=3+
      |a a b b
      |c d d e
      |c f f g
      |c h h g""".stripMargin,
    """2 1 4 3
      |4 3 2 1
      |1 4 3 2
      |3 2 1 4""".stripMargin
    )

  behavior of kenken2_24x4_puzzle

  it should "have solution\n" + kenken2_4x4_solution in {
    expect(List(Grid(kenken2_4x4_solution))) {
      KenKen(kenken2_24x4_puzzle).solutions
    }
  }

  it should "print as\n" + kenken2_24x4_puzzle in {
    expect(kenken2_24x4_puzzle) {
      KenKen(kenken2_24x4_puzzle).toString
    }
  }


  val (kenken3_4x4_puzzle, kenken3_4x4_solution) = (
    """a=12+ b=3+ c=7+ d=9+ e=5+ f=4+
      |a a a b
      |c a d b
      |c e d d
      |c e f f""".stripMargin,
    """3 1 4 2
      |2 4 3 1
      |1 3 2 4
      |4 2 1 3""".stripMargin
    )

  behavior of kenken3_4x4_puzzle

  it should "have solution\n" + kenken3_4x4_solution in {
    expect(List(Grid(kenken3_4x4_solution))) {
      KenKen(kenken3_4x4_puzzle).solutions
    }
  }

  it should "print as\n" + kenken3_4x4_puzzle in {
    expect(kenken3_4x4_puzzle) {
      KenKen(kenken3_4x4_puzzle).toString
    }
  }


  val (kenken_6x6_puzzle, kenken_6x6_solution) = (
    """a=24x b=13+ c=5 d=6x e=7+ f=30x g=10+ h=15+ i=12+ j=1- k=9+ l=24x m=3- n=6
      |a a b c d d
      |e e b b d f
      |g e b h h f
      |g i i j h f
      |g i k j l l
      |m m k k n l""".stripMargin,
    """6 4 2 5 3 1
      |3 1 6 4 2 5
      |5 3 1 6 4 2
      |1 6 4 2 5 3
      |4 2 5 3 1 6
      |2 5 3 1 6 4""".stripMargin
    )

  behavior of kenken_6x6_puzzle

  it should "have solution\n" + kenken_6x6_solution in {
    expect(List(Grid(kenken_6x6_solution))) {
      KenKen(kenken_6x6_puzzle).solutions
    }
  }

  it should "print as\n" + kenken_6x6_puzzle in {
    expect(kenken_6x6_puzzle) {
      KenKen(kenken_6x6_puzzle).toString
    }
  }
}
