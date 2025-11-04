package rational

import org.scalatest.funsuite.AnyFunSuite
import rational.immutable.Rational

class ScoredRationalSuite extends AnyFunSuite {

  test("5/1 should only print its numerator") {
    val stream = java.io.ByteArrayOutputStream()
    val result = Rational(5)
    Console.withOut(stream) {
      result.print()
    }
    // this will be \n under linux and \r\n under windows
    val linesep = sys.props("line.separator")
    assert(s"5$linesep" == stream.toString())
  }

  test("1/5 should print numerator and denominator") {
    val stream = java.io.ByteArrayOutputStream()
    val result = Rational(1, 5)
    Console.withOut(stream) {
      result.print()
    }
    val linesep = sys.props("line.separator")
    assert(s"1/5$linesep" == stream.toString())
  }

  test("reciprocal of 4/13 should equal 13/4") {
    val x = Rational(4, 13)
    val y = x.reciprocal
    assert(y.numerator == 13)
    assert(y.denominator == 4)
  }

  test("reciprocal of 0/69 should throw an ArithmeticException") {
    assertThrows[ArithmeticException] {
      val x = Rational(0, 69)
      x.reciprocal
    }
  }

  test("-(79/139) should equal -79/139") {
    val x = Rational(79, 139)
    val y = -x
    assert(y.numerator == -79)
    assert(y.denominator == 139)
  }

  test("3/4 minus 1/2 should be 1/4") {
    val x = Rational(3, 4)
    val y = Rational(1, 2)
    val z = x - y
    assert(z.numerator == 1)
    assert(z.denominator == 4)
  }

  test("3/4 minus -1/2 should be 5/4") {
    val x = Rational(3, 4)
    val y = Rational(-1, 2)
    val z = x - y
    assert(z.numerator == 5)
    assert(z.denominator == 4)
  }

  test("5/2 minus 0/69 should equal 5/2") {
    val x = Rational(5, 2)
    val y = Rational(0, 69)
    val z = x - y
    assert(z.numerator == x.numerator)
    assert(z.denominator == x.denominator)
  }

  test("6/5 minus 1 should equal 1/5") {
    val x = Rational(6, 5)
    val y = 1
    val z = x - y
    assert(z.numerator == 1)
    assert(z.denominator == 5)
  }

  test("3/2 minus 2/-1 should equal 7/2") {
    val x = Rational(3, 2)
    val y = Rational(2, -1)
    val z = x - y
    assert(z.numerator == 7)
    assert(z.denominator == 2)
  }

  test("3/4 times 8/5 should equal 6/5") {
    val x = Rational(3, 4)
    val y = Rational(8, 5)
    val z = x * y
    assert(z.numerator == 6)
    assert(z.denominator == 5)
  }

  test("5/6 times 0/3 should equal 0/3") {
    val x = Rational(5, 6)
    val y = Rational(0, 3)
    val z = x * y
    assert(z.numerator == 0)
    assert(z.denominator == 1)
  }

  test("5/6 times 23/23 should equal 5/6") {
    val x = Rational(5, 6)
    val y = Rational(23, 23)
    val z = x * y
    assert(z.numerator == x.numerator)
    assert(z.denominator == x.denominator)
  }

  test("24/13 divided by 2/3 should equal 36/13") {
    val x = Rational(24, 13)
    val y = Rational(2, 3)
    val z = x / y
    assert(z.numerator == 36)
    assert(z.denominator == 13)
  }

  test("24/13 divided 0/4 should throw an ArithmeticException") {
    assertThrows[ArithmeticException] {
      val x = Rational(24, 13)
      val y = Rational(0, 4)
      x / y
    }
  }

  test("24/13 divided by 69/69 should equal 24/13") {
    val x = Rational(24, 13)
    val y = Rational(69, 69)
    val z = x / y
    assert(z.numerator == x.numerator)
    assert(z.denominator == x.denominator)
  }

  test("The minimum of 1/5 and 1/7 should be 1/7") {
    val result = Rational(1, 5) min Rational(1, 7)
    assert(result.numerator == 1)
    assert(result.denominator == 7)
  }

  test("The minimum of -1/5 and 1/7 should be -1/5") {
    val result = Rational(-1, 5) min Rational(1, 7)
    assert(result.numerator == -1)
    assert(result.denominator == 5)
  }

  test("The minimum of 0 and 1/100 should be 0") {
    val result = Rational(0) min Rational(1, 100)
    assert(result.numerator == 0)
    assert(result.denominator == 1)
  }

  test("1/100 should have a correct string representation") {
    val result = Rational(1, 100).toString
    assert(result == "1/100")
  }

  test("5/1 should have a correct string representation") {
    val result = Rational(5, 1).toString
    assert(result == "5")
  }

  test("0/100 should have a correct string representation") {
    val result = Rational(0, 100).toString
    assert(result == "0")
  }
}
