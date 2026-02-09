package datastructures

import scala.collection.mutable

class Counter[A](initialElements: Iterable[A]) {


  /** Sets the count for the specified key to the specified value.
    *
    * @throws IllegalArgumentException if the specified count is negative
    */
  def setCount(key: A, count: Long): Unit = ???

  /** Modifies the count for the specified key by adding the specified amount. Count cannot drop below zero. */
  def modifyCount(key: A, amount: Long): Unit = ???

  /** Returns the count for the specified key. */
  def getCount(key: A): Long = ???

  /** Increments the count by one for each of the specified keys. */
  def incrementCounts(keys: Iterable[A]): Unit = ???

  /** Decrements the count by one for each of the specified keys. Counts cannot drop below zero. */
  def decrementCounts(keys: Iterable[A]): Unit = ???


  override def toString: String = ???

  override def equals(obj: Any): Boolean = ???

  override def hashCode: Int = ???
}
