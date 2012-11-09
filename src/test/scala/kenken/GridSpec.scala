package kenken

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[kenken.Grid]] object.
 */
class GridSpec extends FlatSpec {
  "A grid" can "be created from a string" in {
    val g = Grid(
      """12 23
        |13 123""".stripMargin)
    expect(Set(1, 2)) {
      g((1, 1))
    }
    expect(Set(2, 3)) {
      g((1, 2))
    }
    expect(Set(1, 3)) {
      g((2, 1))
    }
    expect(Set(1, 2, 3)) {
      g((2, 2))
    }
  }
}
