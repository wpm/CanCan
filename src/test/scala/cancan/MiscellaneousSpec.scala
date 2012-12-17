package cancan

import org.scalatest.FlatSpec

/**
 * Miscellaneous unit tests.
 */
class MiscellaneousSpec extends FlatSpec {
  "A matrix of strings" should "print neatly" in {
    expect( """ cat    dog   mouse
              |apple  banana  pear
              | red    blue  yellow""".stripMargin) {
      matrixToString(List(
        List("cat", "dog", "mouse"),
        List("apple", "banana", "pear"),
        List("red", "blue", "yellow")))
    }
  }
}
