package kenken

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[kenken.Constraint]] objects.
 */
class ConstraintSpec extends FlatSpec {
  "A definiteness constraint on row 2 of a 3x3 puzzle" should "have the string representation 'Definite: Row 2'" in {
    val row2Definite = DefinitenessConstraint(Seq((2, 1), (2, 2), (2, 3)))
    expect("Definite: Row 2") {
      row2Definite.toString()
    }
  }

  "A definiteness constraint on col 2 of a 3x3 puzzle" should "have the string representation 'Definite: Col 2'" in {
    val row2Definite = DefinitenessConstraint(Seq((1, 2), (2, 2), (3, 2)))
    expect("Definite: Col 2") {
      row2Definite.toString()
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
      val c = DefinitenessConstraint(Grid.row(6)(1))
      c(g)
    }
  }
}
