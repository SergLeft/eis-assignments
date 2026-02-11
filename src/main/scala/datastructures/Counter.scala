package datastructures

import scala.collection.mutable

class Counter[A](initialElements: Iterable[A]) {

  /** Internal storage mapping each element to its count */
  private val counts: mutable.HashMap[A, Long] = mutable.HashMap()

  // Constructor body so as to count all initial elements
  incrementCounts(initialElements)


  /** Sets the count for the specified key to the specified value.
   *
   * @param key the element whose count is to be set
   * @param count the new count for the specified element
   * @throws IllegalArgumentException if the specified count is negative
   */
  def setCount(key: A, count: Long): Unit = {
    if (count < 0)
      throw IllegalArgumentException("Count CANNOT be negative!")

    if (count == 0)
      counts.remove(key)       // ← REMOVE instead of storing 0
    else
      counts.update(key, count)
  }

  /** Modifies the count for the specified key by adding the specified amount. Count cannot drop below zero.
   * @param key the element whose count is to be modified
   * @param amount the amount by which the count is to be modified
   */
  def modifyCount(key: A, amount: Long): Unit = {
    val newCount = math.max(0, getCount(key) + amount)
    if (newCount == 0)
      counts.remove(key)       // ← REMOVE instead of storing 0
    else
      counts.update(key, newCount)
  }

  /** Returns the count for the specified key.
   * Unknown elements have a count of zero
   *
   * @param key the element whose count is to be returned
   * @return the count for the specified element
   */
  def getCount(key: A): Long = counts.getOrElse(key, 0L)

  /** Increments the count by one for each of the specified keys.
   *
   * @param keys the elements whose counts are to be incremented
   */
  def incrementCounts(keys: Iterable[A]): Unit = {
    keys.foreach(key => modifyCount(key, 1))
  }

  /** Decrements the count by one for each of the specified keys. Counts cannot drop below zero.
   *
   * @param keys the elements whose counts are to be decremented
   */
  def decrementCounts(keys: Iterable[A]): Unit = {
    keys.foreach(key => modifyCount(key, -1))
  }


  /** Returns a string-representation of the counter */
  override def toString: String = counts.toString

  /** Checks if counter equals another object. Two counters are equal iff all elements have same counts
   *
   * @param obj the object to compare with
   * @return true iff the two counters are equal, false otherwise*/
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Counter[?] => this.counts == other.counts
      case _ => false
    }
  }

  /** Returns the hashcode of the counter */
  override def hashCode: Int = counts.hashCode
}