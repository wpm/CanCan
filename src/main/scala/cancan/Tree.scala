package cancan

import collection.TraversableView

case class Node[T](label: T, ns: Node[T]*)

case class Tree[T](root: Node[T]) extends TraversableView[T, Traversable[_]] {
  protected def underlying = null

  def foreach[U](f: (T) => U) {
    def dfs(r: Node[T]): TraversableView[T, Traversable[_]] = {
      println("dfs(" + r.label + ")")
      Traversable(r.label).view ++ r.ns.flatMap(dfs(_))
    }
    dfs(root).foreach(f)
  }
}
