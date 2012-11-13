package kenken

import org.scalatest.FlatSpec

class ParsersSpec extends FlatSpec {
  "A table of strings" should "print neatly" in {
    expect(
      """   cat    dog  mouse
        | apple banana   pear
        |   red   blue yellow""".stripMargin) {
      Parsers.tableToString(List(
        List("cat", "dog", "mouse"),
        List("apple", "banana", "pear"),
        List("red", "blue", "yellow")))
    }
  }
}
