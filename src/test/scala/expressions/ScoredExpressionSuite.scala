package expressions

import expressions.{BinaryOperation, Constant, UnaryOperation, Variable}
import org.scalatest.funsuite.AnyFunSuite

class ScoredExpressionSuite extends AnyFunSuite {

  private val sampleExpression = BinaryOperation(
    "+",
    UnaryOperation("invert", Variable("x")),
    BinaryOperation(
      "*",
      Constant(2),
      UnaryOperation("abs", Variable("y"))
    )
  )

  test("the addition of 2 constants should get simplified to a single constant of the sum") {
    val result = simplified(BinaryOperation("+", Constant(69), Constant(42)))
    assert(result == Constant(111))
  }

  test("the multiplication of 2 constants should get simplified to a single constant of the product") {
    val result = simplified(BinaryOperation("*", Constant(42), Constant(2)))
    assert(result == Constant(84))
  }

  test("the negation of a constant should get simplified to a constant of the negation") {
    val result = simplified(UnaryOperation("-", Constant(42)))
    assert(result == Constant(-42))
  }

  test("the abs of a constant should get simplified to a constant of the abs") {
    val result = simplified(UnaryOperation("abs", Constant(-42)))
    assert(result == Constant(42))
  }

  test("the inverse of a constant should get simplified to a constant of the invert") {
    val result = simplified(UnaryOperation("invert", Constant(42)))
    assert(result == Constant(1.0 / 42))
  }

  test("the inverse of constant 0 should result in an error") {
    assertThrows[ArithmeticException] {
      simplified(UnaryOperation("invert", Constant(0)))
    }
  }

  test("the addition of a term T with the constant 0 should get simplified to T") {
    val result = simplified(BinaryOperation("+", sampleExpression, Constant(0)))
    assert(result == sampleExpression)
  }

  test("the multiplication of a term T with the constant 1 should get simplified to T") {
    val result = simplified(BinaryOperation("*", Constant(1), sampleExpression))
    assert(result == sampleExpression)
  }

  test("the multiplication of a term with the constant 0 should get simplified to the constant 0") {
    assert(simplified(BinaryOperation("*", sampleExpression, Constant(0))) == Constant(0))
    assert(simplified(BinaryOperation("*", Constant(0), sampleExpression)) == Constant(0))
  }

  test("the multiplication of a term T with the constant -1 should get simplified to the unary negation of T") {
    val result = simplified(BinaryOperation("*", sampleExpression, Constant(-1)))
    assert(result == UnaryOperation("-", sampleExpression))
  }

  test("double negation of a term T should get simplified to T") {
    val result = simplified(UnaryOperation("-", UnaryOperation("-", sampleExpression)))
    assert(result == sampleExpression)
  }

  test("double abs of a term T should get simplified to a single abs of T") {
    val result = simplified(UnaryOperation("abs", UnaryOperation("abs", sampleExpression)))
    assert(result == UnaryOperation("abs", sampleExpression))
  }

  test("double inversion of a term should not be simplified away") {
    val expression = UnaryOperation("invert", UnaryOperation("invert", Variable("y")))
    assert(simplified(expression) == expression)
  }
}
