package com.github.wpm.cancan

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[com.github.wpm.cancan.Constraint]] objects.
 */
class ConstraintSpec extends FlatSpec {
  behavior of "A 5-cell preemptive set constraint"

  def r5Fixture = PreemptiveSetConstraint(Markup.row(5)(1))

  it should "have size 5" in {
    expect(5) {
      r5Fixture.size
    }
  }

  it should "handle a single solved cell" in {
    expect(Some(List(
      (Cell(1, 2), Set(2, 3, 4, 5)),
      (Cell(1, 3), Set(2, 3, 4, 5)),
      (Cell(1, 4), Set(2, 3, 4, 5)),
      (Cell(1, 5), Set(2, 3, 4, 5))))) {
      val m = Markup(
        """1 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      r5Fixture(m)
    }
  }

  it should "handle multiple preemptive sets" in {
    expect(Some(List((Cell(1, 5), Set(5))))) {
      val m = Markup(
        """12 12 34 34 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      r5Fixture(m)
    }
  }

  it should "do nothing if there are no preemptive sets" in {
    expect(Some(Nil)) {
      val m = Markup(
        """12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      r5Fixture(m)
    }
  }

  behavior of "A 6-cell preemptive set constraint"

  def preemptiveFixture = PreemptiveSetConstraint(Markup.row(6)(1))

  it should "have size 6" in {
    expect(6) {
      preemptiveFixture.size
    }
  }

  it should "remove invalid candidates" in {
    expect(Some(List(
      (Cell(1, 2), Set(4)),
      (Cell(1, 5), Set(5, 6)),
      (Cell(1, 6), Set(5, 6))
    ))) {
      preemptiveFixture(Markup(
        """123 1234 12 23 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456""".stripMargin))
    }
  }

  it should "do nothing to an invalid markup" in {
    expect(Some(Nil)) {
      preemptiveFixture(Markup(
        """1 1 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456""".stripMargin))
    }
  }

  behavior of "An all-different constraint"

  it should "detect an invalid configuration with multiple solved cells" in {
    expect(None) {
      val m = Markup(
        """1 12345 12345 12345 1
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      AllDifferentConstraint(Markup.row(5)(1))(m)
    }
  }

  it should "leave the row (23 5 6 4 3 2) unchanged" in {
    expect(Some(Nil)) {
      val m = Markup(
        """23 5 6 4 3 2
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456""".stripMargin)
      AllDifferentConstraint(Markup.row(6)(1))(m)
    }
  }

  "A uniqueness constraint on row 2 of a 3x3 puzzle" should "have the string representation 'Uniqueness: Row 2'" in {
    expect("Uniqueness: Row 2") {
      UniquenessConstraint(Seq(Cell(2, 1), Cell(2, 2), Cell(2, 3))).toString()
    }
  }

  "A uniqueness constraint on col 2 of a 3x3 puzzle" should "have the string representation 'Uniquenss: Col 2'" in {
    expect("Uniqueness: Col 2") {
      UniquenessConstraint(Seq(Cell(1, 2), Cell(2, 2), Cell(3, 2))).toString()
    }
  }

  behavior of "A x6 constraint on three cells"
  val times6 = TimesConstraint(6, List(Cell(1, 1), Cell(1, 2), Cell(2, 1)))

  it should "have a size 3" in {
    expect(3) {
      times6.size
    }
  }

  it should "constrain 1234 1234 1234 to 123 123 123" in {
    expect(Some(List((Cell(1, 1), Set(1, 2, 3)), (Cell(1, 2), Set(1, 2, 3)), (Cell(2, 1), Set(1, 2, 3))))) {
      times6(Markup(4))
    }
  }

  it should "stringify as '6x' and '*\t6\tA1 A2 B1'" in {
    expect("6x") {
      times6.toString()
    }

    expect("*\t6\tA1 A2 B1") {
      times6.toNekNekString
    }
  }

  behavior of "A +5 constraint on three cells"
  val plus5 = PlusConstraint(5, List(Cell(1, 1), Cell(1, 2), Cell(2, 1)))

  it should "have a size 3" in {
    expect(3) {
      times6.size
    }
  }

  it should "constrain 1234 1234 1234 to 123 123 123" in {
    expect(Some(List((Cell(1, 1), Set(1, 2, 3)), (Cell(1, 2), Set(1, 2, 3)), (Cell(2, 1), Set(1, 2, 3))))) {
      plus5(Markup(4))
    }
  }

  it should "constrain 1 23 123 to 1 23 123" in {
    expect(Some(Nil)) {
      plus5(Markup(4) ++ List((Cell(1, 1), Set(1)), (Cell(1, 2), Set(2, 3)), (Cell(2, 1), Set(1, 2))))
    }
  }

  it should "stringify as '5+' and '*\t5\tA1 A2 B1'" in {
    expect("5+") {
      plus5.toString()
    }

    expect("+\t5\tA1 A2 B1") {
      plus5.toNekNekString
    }
  }
}
