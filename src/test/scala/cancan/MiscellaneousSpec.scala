package cancan

import org.scalatest.FlatSpec

/**
 * Miscellaneous unit tests.
 */
class MiscellaneousSpec extends FlatSpec {
  "A table of strings" should "print neatly" in {
    expect(
      """   cat    dog  mouse
        | apple banana   pear
        |   red   blue yellow""".stripMargin) {
      tableToString(List(
        List("cat", "dog", "mouse"),
        List("apple", "banana", "pear"),
        List("red", "blue", "yellow")))
    }
  }
}