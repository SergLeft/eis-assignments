package datastructures

import scala.annotation.tailrec

import datastructures.UnbalancedSearchTree.*


object UnbalancedSearchTree {
  private final case class Node[K, V](
    var key: K,
    var value: V,
    var left: Option[Node[K, V]],
    var right: Option[Node[K, V]]
  )

  private def leaf[K, V](key: K, value: V): Node[K, V] = Node(key, value, None, None)

  def apply[K, V](ord: Ordering[K]): UnbalancedSearchTree[K, V] = {
    new UnbalancedSearchTree[K, V](ord)
  }

  def apply[K, V](elements: Iterable[(K, V)])(ord: Ordering[K]): UnbalancedSearchTree[K, V] = {
    val set = new UnbalancedSearchTree[K, V](ord)
    elements.foreach(set.addOrUpdate)
    set
  }
}


class UnbalancedSearchTree[K, V](ord: Ordering[K]) {

  private var root: Option[Node[K, V]] = None

  def size: Int = ???

  def clear(): Unit = ???

  def addOrUpdate(key: K, value: V): Unit = ???


  def get(key: K): Option[V] = ???

  def apply(key: K): V = ???


}
