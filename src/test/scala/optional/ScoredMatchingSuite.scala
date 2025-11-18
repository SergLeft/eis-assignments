package optional

import scala.collection.mutable.ArrayBuffer

import org.scalatest.funsuite.AnyFunSuite

import matching.*

class ScoredMatchingSuite extends AnyFunSuite{
  
  test("mapping None with the string length function should result in None") {
    assert(map[String, Int](string => string.length, None).isEmpty)
  }

  test("mapping None with the head function of ArrayBuffer should result in None") {
    assert(map[ArrayBuffer[Double], Double](buf => buf.head, None).isEmpty)
  }

  test("mapping Some(name) with the string size function should result in Some containing the size of name") {
    val name = "Grace Hopper"
    assert(map[String, Int](s => name.length, Some(name)) == Some(name.length))
  }

  test("mapping Some(buffer) with the min function of ArrayBuffer should result in Some containing the smallest element of buffer") {
    val buffer = ArrayBuffer[Int](10, 76, -43, 89)
    assert(map[ArrayBuffer[Int], Int](buf => buf.min, Some(buffer)) == Some(buffer.min))
  }

  test("None should not satisfy a condition that always succeeds") {
    assert(!satisfies[String](anything => true, None))
  }

  test("Some(string) should satisfy size > 5 when the size of string > 5") {
    assert(satisfies[String](s => s.length > 5, Some("Database Systems")))
  }

  test("Some(-49) should not satisfy equality with 86") {
    assert(!satisfies[Long](number => number == 86, Some(-49)))
  }

  test("Some(-49) should satisfy inequality with 86") {
    assert(satisfies[Long](number => number != 86, Some(-49)))
  }

  test("getOrElse with None should result in the evaluated alternative") {
    val alternative = ArrayBuffer(0)
    assert(getOrElse[ArrayBuffer[Int]](() => alternative, None) == alternative)
  }

  test("getOrElse with Some(name) should return the name") {
    val name = "Products"
    assert(getOrElse[String](() => "No Table", Some(name)) == name)
  }

  test("getOrElse with Some(number) should return the number") {
    val number = -182
    assert(getOrElse[Int](() => 0, Some(number)) == number)
  }

  test("getOrElse should not evaluate the alternative if a value exists") {
    assert(getOrElse[Long](() => throw IllegalArgumentException("Data must not be empty"), Some(25)) == 25)
  }

  test("both with arguments None and None should result in None") {
    assert(both[Double, Int](None, None).isEmpty)
  }

  test("both with arguments None and Some(21) should result in None") {
    assert(both[Int, Int](None, Some(21)).isEmpty)
  }

  test("both with arguments Some(name) and None should result in None") {
    val name = "Ada Lovelace"
    assert(both[String, ArrayBuffer[Long]](Some(name), None).isEmpty)
  }

  test("both with arguments Some(age) and Some(name) should result in Some((age, name))") {
    val name = "Edsger Dijkstra"
    val age = 37
    assert(both[Int, String](Some(age), Some(name)) == Some((age, name)))
  }

  test("concatenating 2 transformations which return None should result in one returning None") {
    val concatenated = concat[Long, String, Int](l => None, s => None)
    assert(concatenated(254).isEmpty)
  }

  test("concatenating 2 transformations where the second returns None should result in one returning None") {
    val concatenated = concat[Long, String, Int](l => Some(l.toString), s => None)
    assert(concatenated(254).isEmpty)
  }

  test("concatenating 2 transformations where the first returns None should result in one returning None") {
    val concatenated = concat[Long, String, Int](l => None, s => Some(s.length))
    assert(concatenated(254).isEmpty)
  }

  test("concatenating 2 transformations where both return Some the result should be one returning Some with the final result") {
    val concatenated = concat[Long, String, Int](l => Some(l.toString), s => Some(s.length))
    assert(concatenated(254) == Some(3))
  }
}
