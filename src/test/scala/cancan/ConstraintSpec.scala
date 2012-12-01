package cancan

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[cancan.Constraint]] objects.
 */
class ConstraintSpec extends FlatSpec {
  behavior of "A rank constraint"

  def r5Fixture = PermutationSetConstraint(5, Grid.row(5)(1))

  it should "handle a single solved cell" in {
    expect(Some(List(
      (Cell(1, 2), Set(2, 3, 4, 5)),
      (Cell(1, 3), Set(2, 3, 4, 5)),
      (Cell(1, 4), Set(2, 3, 4, 5)),
      (Cell(1, 5), Set(2, 3, 4, 5))))) {
      val g = Grid(
        """1 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      r5Fixture(g)
    }
  }

  it should "handle multiple pemutation sets" in {
    expect(Some(List((Cell(1, 5), Set(5))))) {
      val g = Grid(
        """12 12 34 34 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      r5Fixture(g)
    }
  }

  it should "do nothing if there are no permuation sets" in {
    expect(Some(Nil)) {
      val g = Grid(
        """12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      r5Fixture(g)
    }
  }

  it should "detect an invalid configuration with multiple permutation sets" in {
    expect(None) {
      val g = Grid(
        """12 12 34 34 1234
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      r5Fixture(g)
    }
  }

  it should "detect an invalid configuration with multiple solved cells" in {
    expect(None) {
      val g = Grid(
        """1 12345 12345 12345 1
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345
          |12345 12345 12345 12345 12345""".stripMargin)
      r5Fixture(g)
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

  "A permustation set constraint on a row (23 5 6 4 3 2)" should "be inconsistent" in {
    expect(None) {
      val g = Grid(
        """23 5 6 4 3 2
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456""".stripMargin)
      val c = PermutationSetConstraint(6, Grid.row(6)(1))
      c(g)
    }
  }


  "A x12 constraint on three cells" should "Stringify as '12x' and '*\t12\tA1 A2 B1'" in {
    val times12 = TimesConstraint(12, List(Cell(1, 1), Cell(1, 2), Cell(2, 1)))
    expect("12x") {
      times12.toString()
    }

    expect("*\t12\tA1 A2 B1") {
      times12.toNekNekString
    }
  }
}
