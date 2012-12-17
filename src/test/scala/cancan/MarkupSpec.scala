package cancan

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[cancan.Markup]] object.
 */
class MarkupSpec extends FlatSpec {
  val m2x2 = """12 2
               |1  12""".stripMargin

  behavior of "A markup"

  it can "be created from string" in {
    val m = Markup(m2x2)
    expect(Set(1, 2)) {
      m(Cell(1, 1))
    }
    expect(Set(2)) {
      m(Cell(1, 2))
    }
    expect(Set(1)) {
      m(Cell(2, 1))
    }
    expect(Set(1, 2)) {
      m(Cell(2, 2))
    }
  }

  it should "support lookup by integer tuple" in {
    val m = Markup(m2x2)
    expect(Set(1, 2)) {
      m(1, 1)
    }

    expect(Set(2)) {
      m(1, 2)
    }
    expect(Set(1)) {
      m(2, 1)
    }
    expect(Set(1, 2)) {
      m(2, 2)
    }
  }

  it should "support the assignment operator" in {
    val m = Markup(m2x2)
    expect(Markup( """12  2
                     |12 12""".stripMargin)) {
      m((2, 1)) = Set(1, 2)
    }
  }

  it should "support the + operator" in {
    val m = Markup(m2x2)
    expect(Markup( """12  2
                     |12 12""".stripMargin)) {
      m + (Cell(2, 1) -> Set(1, 2))
    }
  }

  it should "support the ++ operator" in {
    val m = Markup(m2x2)
    expect(Markup( """12  2
                     |12 2""".stripMargin)) {
      m ++ List(Cell(2, 1) -> Set(1, 2), Cell(2, 2) -> Set(2))
    }

    expect(Markup( """1 23 1234 1234
                     |12 1234 1234 1234
                     |1234 1234 1234 1234
                     |1234 1234 1234 1234""".stripMargin)) {
      Markup(4) ++ List(Cell(1, 1) -> Set(1), Cell(1, 2) -> Set(2, 3), Cell(2, 1) -> Set(1, 2))
    }
  }

  it should "be printed as\n" + m2x2 in {
    expect(Markup(m2x2).toString) {
      m2x2
    }
  }

  it should "have unsolved cells (1,1) and (2,2)" in {
    expect(Vector((Cell(1, 1), Set(1, 2)), (Cell(2, 2), Set(1, 2)))) {
      Markup(m2x2).unsolved
    }
  }

  "A period" should "represent an empty cell" in {
    expect(Set.empty) {
      Markup( """1 2
                |. 1""".stripMargin)(2, 1)
    }
  }

  "Row 3 of a 4x4 markup" should "contain cells (3,1), (3,2), (3,3), and (3,4)" in {
    expect(List(Cell(3, 1), Cell(3, 2), Cell(3, 3), Cell(3, 4))) {
      Markup.row(4)(3)
    }
  }

  "Column 3 of a 4x4 markup" should "contain cells (1,3), (2,3), (3,3), and (4,3)" in {
    expect(List(Cell(1, 3), Cell(2, 3), Cell(3, 3), Cell(4, 3))) {
      Markup.col(4)(3)
    }
  }
}
