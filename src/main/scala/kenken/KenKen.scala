package kenken

import collection.SeqView
import collection.mutable

object KenKen {

  def solve(grid: Grid, visited: mutable.Set[Grid] = mutable.Set[Grid]()): SeqView[Grid, List[Grid]] = {
    /**
     * This returns a list of grids with guesses in all the unsolved cells.
     */
    def search(grid: Grid) = {
      grid.unsolvedCells.flatMap(u => u._2.map(value => (u._1 -> Set(value)))).map(grid + _).toList
    }

    (grid.isSolved match {
      case true => if (visited.contains(grid)) Nil
      else {
        visited += grid
        grid :: Nil
      }
      case false => search(grid).flatMap(solve(_, visited))
    }).view
  }

  def main(args: Array[String]) {
    val n = 2
    val p = Puzzle(n)
    val g = Grid(n)
//    val a = propagateConstraints(p, g)
//    println(a)
    //    val s = solve(g)
    //    println(s.mkString("\n\n"))
    //    println(s.length)
  }
}
