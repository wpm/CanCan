package cancan

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[cancan.Cell]] objects.
 */
class CellSpec extends FlatSpec {
  behavior of "Cell (3,4)"
  it should "print as an ordered pair" in {
    expect("(3,4)") {
      Cell(3, 4).toString
    }
  }
  it should "have the NekNek representation C4" in {
    expect("C4") {
      Cell(3, 4).toNekNekString
    }
  }

  "Cells" should "sort by row then column" in {
    expect(List((1, 3), (1, 5), (2, 3), (2, 3), (3, 1))) {
      List((1, 5), (2, 3), (1, 3), (3, 1), (2, 3)).sorted
    }

    expect(List(Cell(3, 2), Cell(4, 2), Cell(4, 3))) {
      List(Cell(4, 3), Cell(4, 2), Cell(3, 2)).sorted
    }
  }
}
