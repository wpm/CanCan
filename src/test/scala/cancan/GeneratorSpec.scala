package cancan

import org.scalatest.FlatSpec

/**
 * Unit tests for the [[cancan.Generator]] object.
 */
class GeneratorSpec extends FlatSpec {
  "The connected components of a->bc b->ac c->ab d->e e->d f" should "be abc, de, f" in {
    expect(Set(Set('a, 'b, 'c), Set('d, 'e), Set('f))) {
      def adjacent[N](edges: Map[N, List[N]])(n: N): List[N] = edges.getOrElse(n, Nil)
      val edges = Map('a -> List('b, 'c), 'b -> List('a, 'c), 'c -> List('a, 'b),
        'd -> List('e), 'e -> List('d), 'f -> Nil)
      val nodes = edges.keys ++ edges.values.flatMap(x => x)
      Set() ++ Generator.connectedComponents(nodes, adjacent(edges)(_: Symbol), 3)
    }
  }

  "A random puzzle with a unique solution" should "have only the solution reported for it" in {
    val (puzzle, solution) = Generator.uniqueRandomPuzzle(5)
    val actual = solutions(puzzle).force
    expect(1, "Invalid number of solutions for\n" + puzzle) {
      actual.size
    }
    expect(List[Markup](solution), "Invalid solution for\n" + puzzle) {
      actual
    }
  }
}
