package dbms.v2

import expressions.{BinaryOperation, Constant, UnaryOperation, Variable}
import org.scalatest.funsuite.AnyFunSuite
import dbms.v2.store.*
import dbms.v2.misc.*

class ScoredComputationTableSuite extends AnyFunSuite {

  private val schema = Schema(
    Seq(
      "name" -> DBType.String,
      "price" -> DBType.Double,
      "amount" -> DBType.Long,
      "discountPercentage" -> DBType.Double
    )
  )
  private val table = ComputationTable(
    schema,
    Seq(
      TableRecord(Seq("name" -> Variant("Apple"), "price" -> Variant(1.25), "amount" -> Variant(2500), "discountPercentage" -> Variant(0.01))),
      TableRecord(Seq("name" -> Variant("Orange"), "price" -> Variant(1.2), "amount" -> Variant(1200), "discountPercentage" -> Variant(0.0))),
      TableRecord(Seq("name" -> Variant("Mango"), "price" -> Variant(2.1), "amount" -> Variant(975), "discountPercentage" -> Variant(0.05))),
      TableRecord(Seq("name" -> Variant("Chili"), "price" -> Variant(1.9), "amount" -> Variant(3333), "discountPercentage" -> Variant(0.12))),
      TableRecord(Seq("name" -> Variant("Walnut"), "price" -> Variant(0.99), "amount" -> Variant(4566), "discountPercentage" -> Variant(0.07)))
    )
  )
  
  private def emptyTable: ComputationTable = ComputationTable(schema)

  private val price = Variable("price")
  private val totalPriceNoDiscount = BinaryOperation("*", Variable("price"), Variable("amount"))
  private val totalPrice = BinaryOperation(
    "*",
    totalPriceNoDiscount,
    BinaryOperation(
      "+",
      Constant(1),
      UnaryOperation("-", Variable("discountPercentage"))
    )
  )
  private val invalid1 = BinaryOperation(
    "+",
    BinaryOperation(
      "*",
      totalPrice,
      Variable("name")
    ),
    totalPrice
  )
  private val invalid2 = UnaryOperation("abs", Variable("supplierID"))
  private val dubious = BinaryOperation(
    "+",
    BinaryOperation(
      "*",
      Variable("amount"),
      UnaryOperation("invert", Variable("discountPercentage"))
    ),
    Constant(100.0)
  )

  test("running the price query should result in the price of every item") {
    val result = table.compute(price)
    val expected = Seq(1.25, 1.2, 2.1, 1.9, 0.99)
    assert(result == expected)
  }

  test("running the query for the total price without the discount should result in the correct prices") {
    val result = table.compute(totalPriceNoDiscount)
    val expected = Seq(3125.0, 1440.0, 2047.5, 6332.7, 4520.34)
    assert(result == expected)
  }

  test("running the query for the total price with the discount should result in the correct prices") {
    val result = table.compute(totalPrice)
    val expected = Seq(3093.75, 1440.0, 1945.125, 5572.776, 4203.9162)
    assert(result == expected)
  }

  test("running the first invalid query should result in an error") {
    assertThrows[IllegalArgumentException] {
      table.compute(invalid1)
    }
  }

  test("running the first invalid query on an empty table should result in an error") {
    assertThrows[IllegalArgumentException] {
      emptyTable.compute(invalid1)
    }
  }

  test("running the second invalid query should result in an error") {
    assertThrows[IllegalArgumentException] {
      table.compute(invalid2)
    }
  }

  test("running the second invalid query on an empty table should result in an error") {
    assertThrows[IllegalArgumentException] {
      emptyTable.compute(invalid2)
    }
  }

  test("when the query throws an error for a row the value 0 should be the result for that row") {
    val result = table.compute(dubious)
    val expected = Seq(250100.0, 0.0, 19600.0, 27875.000000000004, 65328.57142857143)
    assert(result == expected)
  }
}
