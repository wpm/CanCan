package cancan

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[cancan.StringRepresentation]] object.
 */
class StringRepresentationSpec extends FlatSpec {
  "A table of strings" should "print neatly" in {
    expect(
      """   cat    dog  mouse
        | apple banana   pear
        |   red   blue yellow""".stripMargin) {
      StringRepresentation.tableToString(List(
        List("cat", "dog", "mouse"),
        List("apple", "banana", "pear"),
        List("red", "blue", "yellow")))
    }
  }
}
