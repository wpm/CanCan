package kenken

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[kenken.Constraint]] objects.
 */
class ConstraintSpec extends FlatSpec {
  "A definiteness constraint on row 2 of a 3x3 puzzle" should "have the string representation 'Definite: Row 2'" in {
    val row2Definite = DefinitenessConstraint(Vector((2, 1), (2, 2), (2, 3)))
    expect("Definite: Row 2") {
      row2Definite.toString
    }
  }

  "A definiteness constraint on col 2 of a 3x3 puzzle" should "have the string representation 'Definite: Col 2'" in {
    val row2Definite = DefinitenessConstraint(Vector((1, 2), (2, 2), (3, 2)))
    expect("Definite: Col 2") {
      row2Definite.toString
    }
  }
}
