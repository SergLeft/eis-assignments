package rational

import org.scalatest.funsuite.AnyFunSuite

import rational.immutable.*

class RationalSuite extends AnyFunSuite {

  test("4 should equal 4/1") {
    val x = Rational(4)
    assert(x.numerator == 4)
    assert(x.denominator == 1)
  }

  test("6/9 should equal 2/3") {
    val x = Rational(6, 9)
    assert(x.numerator == 2)
    assert(x.denominator == 3)
  }

  test("76/0 should be illegal") {
    assertThrows[IllegalArgumentException] {
      Rational(76, 0)
    }
  }

  test("5/2 plus 15/5 should equal 11/2") {
    val x = Rational(5, 2)
    val y = Rational(15, 5)
    val z = x + y
    assert(z.numerator == 11)
    assert(z.denominator == 2)
  }

  test("5/2 plus 0/69 should equal 5/2") {
    val x = Rational(5, 2)
    val y = Rational(0, 69)
    val z = x + y
    assert(z.numerator == x.numerator)
    assert(z.denominator == x.denominator)
  }

  test("7/3 plus -2/6 should equal 2/1") {
    val x = Rational(7, 3)
    val y = Rational(-2, 6)
    val z = x + y
    assert(z.numerator == 2)
    assert(z.denominator == 1)
  }

  test("5/2 plus 4 should equal 13/2") {
    val x = Rational(5, 2)
    val y = 4
    val z = x + y
    assert(z.numerator == 13)
    assert(z.denominator == 2)
  }

  test("The maximum of 1/5 and 1/7 should be 1/5") {
    val result = Rational(1, 5) max Rational(1, 7)
    assert(result.numerator == 1)
    assert(result.denominator == 5)
  }

  test("The maximum of -1/5 and 1/7 should be 1/7") {
    val result = Rational(-1, 5) max Rational(1, 7)
    assert(result.numerator == 1)
    assert(result.denominator == 7)
  }

  test("The maximum of 0 and 1/100 should be 1/100") {
    val result = Rational(0) max Rational(1, 100)
    assert(result.numerator == 1)
    assert(result.denominator == 100)
  }
}
