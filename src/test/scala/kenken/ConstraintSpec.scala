package kenken

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[kenken.Constraint]] objects.
 */
class ConstraintSpec extends FlatSpec {
  "A solved cell constraint on row 2 of a 3x3 puzzle" should "have the string representation 'Solved: Row 2'" in {
    val row2Solved = SolvedCellsConstraint(Seq(Cell(2, 1), Cell(2, 2), Cell(2, 3)))
    expect("Solved: Row 2") {
      row2Solved.toString()
    }
  }

  "A solved cell constraint on col 2 of a 3x3 puzzle" should "have the string representation 'Solved: Col 2'" in {
    val col2Solved = SolvedCellsConstraint(Seq(Cell(1, 2), Cell(2, 2), Cell(3, 2)))
    expect("Solved: Col 2") {
      col2Solved.toString()
    }
  }

  "A definiteness constraint on a row (23 5 6 4 3 2)" should "be inconsistent" in {
    expect(None) {
      val g = Grid(
        """23 5 6 4 3 2
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456
          |123456 123456 123456 123456 123456 123456""".stripMargin)
      val c = SolvedCellsConstraint(Grid.row(6)(1))
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
