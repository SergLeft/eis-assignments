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

  /** Tracks number of elements, size is O(1) */
  private var numElements: Int = 0

  /** Returns number of key-value pairs in tree
   *
   * @return size of tree
   */
  def size: Int = numElements

  /** Removes all elements from the tree. */
  def clear(): Unit = {
    root = None
    numElements = 0
  }

  /** Adds a new key-value pair or updates value if key already exists.
   *
   * @param key the key to add or update
   * @param value the value to associate with the key
   */
  def addOrUpdate(key: K, value: V): Unit = {
    root match {
      case None =>
        root = Some(leaf(key, value))
        numElements += 1
      case Some(node) =>
        addOrUpdateRec(node, key, value)
    }
  }

  /** Convenience method to add a tuple. Used by companion object's apply method. */
  private def addOrUpdate(tuple: (K, V)): Unit = addOrUpdate(tuple._1, tuple._2)

  /** Recursive helper for addOrUpdate.
   *
   * @param node the current node being examined
   * @param key the key to add or update
   * @param value the value to associate with the key
   */
  @tailrec
  private def addOrUpdateRec(node: Node[K, V], key: K, value: V): Unit = {
    if (ord.lt(key, node.key)) {
      // key < node.key → go left
      node.left match {
        case None =>
          node.left = Some(leaf(key, value))
          numElements += 1
        case Some(leftChild) =>
          addOrUpdateRec(leftChild, key, value)
      }
    } else if (ord.gt(key, node.key)) {
      // key > node.key → go right
      node.right match {
        case None =>
          node.right = Some(leaf(key, value))
          numElements += 1
        case Some(rightChild) =>
          addOrUpdateRec(rightChild, key, value)
      }
    } else {
      // key == node.key → update the value
      node.value = value
    }
  }

  /** Returns the value associated with the given key, or None if not found.
   *
   * @param key the key to search for
   * @return the value associated with the given key, or None if not found
   */
  def get(key: K): Option[V] = {
    @tailrec
    def search(current: Option[Node[K, V]]): Option[V] = {
      current match {
        case None => None
        case Some(node) =>
          if (ord.equiv(key, node.key)) Some(node.value)
          else if (ord.lt(key, node.key)) search(node.left)
          else search(node.right)
      }
    }
    search(root)
  }

  /** Returns the value associated with the given key.
   *
   * @param key the key to search for
   * @return the value associated with the given key
   * @throws NoSuchElementException if the key is not found
   */
  def apply(key: K): V = {
    get(key).getOrElse(throw new NoSuchElementException(s"Key $key not found"))
  }
}