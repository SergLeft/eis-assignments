package datastructures

import org.scalatest.funsuite.AnyFunSuite


class ScoredCounterSuite extends AnyFunSuite {

  private val data = Seq(1, 0, 0, 2, 0, 1, 0, 0, 1, 1, 3)

  test("reading the count of 0 should return 5") {
    val counter = Counter(data)
    assert(counter.getCount(0) == 5)
  }

  test("reading the count of 3 should return 1") {
    val counter = Counter(data)
    assert(counter.getCount(3) == 1)
  }

  test("reading the count of 100 should return 0 as 100 is not in the original data") {
    val counter = Counter(data)
    assert(counter.getCount(100) == 0)
  }

  test("setting the count of 0 to 100 should work as intended") {
    val counter = Counter(data)
    counter.setCount(0, 100)
    assert(counter.getCount(0) == 100)
  }

  test("setting the count of an element that was not previously counted should work") {
    val counter = Counter(data)
    counter.setCount(4, 2)
    assert(counter.getCount(4) == 2)
  }

  test("trying to set the count of 2 to -100 should result in an error") {
    assertThrows[IllegalArgumentException] {
      Counter(data).setCount(2, -100)
    }
  }

  test("modifying the count of 0 by 4 should increase the count of 0 to 9") {
    val counter = Counter(data)
    counter.modifyCount(0, 4)
    assert(counter.getCount(0) == 9)
  }

  test("modifying the count of 0 by -2 should reduce the count of 0 to 3") {
    val counter = Counter(data)
    counter.modifyCount(0, -2)
    assert(counter.getCount(0) == 3)
  }

  test("modifying the count of 0 by -9 should reduce the count of 0 to 0") {
    val counter = Counter(data)
    counter.modifyCount(0, -9)
    assert(counter.getCount(0) == 0)
  }

  test("modifying the count of 100 by 3 should set the count to 3 as 100 was not in the data") {
    val counter = Counter(data)
    counter.modifyCount(100, 3)
    assert(counter.getCount(100) == 3)
  }

  test("incrementCounts should increment the counts of each element in the given iterable") {
    val counter = Counter(data)
    val numbers = Seq(1, 0, 5, 2, 0)
    counter incrementCounts numbers
    val expected = Seq(0 -> 7, 1 -> 5, 2 -> 2, 3 -> 1, 5 -> 1)
    expected.foreach((key, expectedCount) => assert(expectedCount == counter.getCount(key)))
  }

  test("decrementCounts should decrement the counts of each element in the given iterable") {
    val counter = Counter(data)
    val numbers = Seq(1, 0, 3, 0)
    counter decrementCounts numbers
    assert(counter.getCount(0) == 3)
    assert(counter.getCount(1) == 3)
    assert(counter.getCount(3) == 0)
  }

  test("decrementCounts should decrement the counts of each (potentially unknown) element in the given iterable") {
    val counter = Counter(data)
    counter decrementCounts Seq(0, 100)
    assert(counter.getCount(0) == 4)
    assert(counter.getCount(100) == 0)
  }

  test("after decrementCounts counters with and without 0 count keys should be equal") {
    val counter = Counter(data ++ Seq(5))
    val counter2 = Counter(data)
    counter decrementCounts Seq(5)
    assert(counter.hashCode == counter2.hashCode)
    // Equal must be commutative
    assert(counter == counter2)
    assert(counter2 == counter)
  }

  test("setting a the count of a key to 0 should be equal to a table without that key") {
    val counter = Counter(data ++ Seq(5))
    val counter2 = Counter(data)
    counter.setCount(5, 0)
    assert(counter.hashCode == counter2.hashCode)
    assert(counter == counter2)
    assert(counter2 == counter)
  }

  test("2 counters based on the same data should be equal") {
    val counter1 = Counter(data)
    val counter2 = Counter(data)
    assert(counter1.hashCode == counter2.hashCode)
    assert(counter1 == counter2)
    assert(counter2 == counter1)
  }

  test("2 counters should be equal if their counts of each element are equal") {
    val otherData = util.Random(0).shuffle(data)
    val counter1 = Counter(data)
    val counter2 = Counter(otherData)
    assert(counter1.hashCode == counter2.hashCode)
    assert(counter1 == counter2)
    assert(counter2 == counter1)
  }

  test("adding a key and removing it afterwards should result in the same counter as before adding the key") {
    val original = Counter(data)
    val modified = Counter(data)
    modified.modifyCount(10, 50)
    modified.modifyCount(10, -100)
    assert(modified.getCount(10) == original.getCount(10))
    assert(modified.hashCode == original.hashCode)
    assert(modified == original)
    assert(original == modified)
  }

  test("changing a single count should result in unequal counters") {
    val counter1 = Counter(data)
    val counter2 = Counter(data)
    counter2.modifyCount(0, 1)
    assert(counter1 != counter2)
    assert(counter2 != counter1)
  }

  test("counter should also work for strings") {
    val stringData = Seq("a", "b", "a", "c", "a")
    val counter = Counter(stringData)
    assert(counter.getCount("a") == 3)
    assert(counter.getCount("b") == 1)
    counter.modifyCount("a", 1)
    assert(counter.getCount("a") == 4)
  }
}
