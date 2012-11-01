package kenken

import collection.SeqView
import collection.mutable

object KenKen {

  def propagateConstraints(puzzle: Puzzle, grid: Grid): Grid = {
    def propagate(grid: Grid, constraints: mutable.Set[Constraint]): Grid = {
      var g = grid
      while (!constraints.isEmpty) {
        val constraint = constraints.head
        constraints -= constraint
        var r = applyConstraint(constraint, g)
        g = r._1
        constraints ++= r._2
      }
      g
    }
    propagate(grid, mutable.Set(puzzle.constraints.values.flatten.toList: _*))
  }

  def applyConstraint(constraint: Constraint, grid: Grid): (Grid, List[Constraint]) = {
    constraint.map(cell => grid(cell))
    (grid, Nil)
  }

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
    val a = propagateConstraints(p, g)
    println(a)
    //    val s = solve(g)
    //    println(s.mkString("\n\n"))
    //    println(s.length)
  }
}
