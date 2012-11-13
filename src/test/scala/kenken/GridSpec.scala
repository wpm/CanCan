package kenken

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[kenken.Grid]] object.
 */
class GridSpec extends FlatSpec {
  val g2x2 = """12  2
               | 1 12""".stripMargin

  behavior of g2x2

  it can "be created from string" in {
    val g = Grid(g2x2)
    expect(Set(1, 2)) {
      g((1, 1))
    }
    expect(Set(2)) {
      g((1, 2))
    }
    expect(Set(1)) {
      g((2, 1))
    }
    expect(Set(1, 2)) {
      g((2, 2))
    }
  }

  it should "be printed as\n" + g2x2 in {
    expect(Grid(g2x2).toString) {
      g2x2
    }
  }
}
