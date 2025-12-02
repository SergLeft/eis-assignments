package dbms.v1

import org.scalatest.funsuite.AnyFunSuite

/*
class ScoredBulkLoadingSuite extends AnyFunSuite {

  private val schema = Seq("empId", "salary", "bonus")
  private val swizzledSchema = Seq("bonus", "salary", "empId")
  private val superSchema = Seq("hours") ++ schema
  private val subSchema = Seq("empId", "salary")

  private val otherSchema = Seq("managerId", "assets", "bonus")

  private val testRecords = Seq(
    TableRecord(schema.zip(Seq(3.0, 3033.4, 0.4))),
    TableRecord(schema.zip(Seq(5.0, 2943.7, 0.0))),
    TableRecord(schema.zip(Seq(7.0, 845.8, 0.1))),
    TableRecord(schema.zip(Seq(11.0, 1235.3, 2.1))),
    TableRecord(schema.zip(Seq(12.0, 1235.3, 7.1))),
  )

  private val otherRecords = Seq(
    TableRecord(otherSchema.zip(Seq(30.0, 30334.0, 1.4))),
    TableRecord(otherSchema.zip(Seq(50.0, 29437.0, 1.0))),
    TableRecord(otherSchema.zip(Seq(70.0, 8458.0, 1.1))),
    TableRecord(otherSchema.zip(Seq(110.0, 12353.0, 3.1))),
    TableRecord(otherSchema.zip(Seq(120.0, 12353.0, 8.1))),
  )

  private val badRecords = Seq(
    TableRecord(otherSchema.zip(Seq(30.0, 30334.0, 1.4))),
    TableRecord(otherSchema.zip(Seq(50.0, 29437.0, 1.0, 12345.6789))),
    TableRecord(otherSchema.zip(Seq(70.0, 8458.0, 1.1))),
  )

  private def extractAttribute(records: Seq[TableRecord], attribute: String): Seq[Value] = {
    records.map(record => record.getValue(attribute))
  }

  private def checkTable(table: Table, expectedSchema: Seq[String], comparisonRecords: Seq[TableRecord]): Unit = {
    val tableRecords = (0 until table.numRecords).map(i => table.getRecord(i))

    assert(table.schema == expectedSchema)
    assert(table.numRecords == comparisonRecords.size)
    table.schema.foreach{ attribute =>
      assert(extractAttribute(tableRecords, attribute) == extractAttribute(comparisonRecords, attribute))
    }
  }

  test("Empty attributes should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      Table(Seq.empty, testRecords)
    }
  }

  test("Valid data should be accepted") {
    val table = Table(schema, testRecords)
    checkTable(table, schema, testRecords)
  }

  test("Valid data should be accepted even if the schema is in a different order") {
    val table = Table(swizzledSchema, testRecords)
    checkTable(table, swizzledSchema, testRecords)
  }

  test("Different valid data should also be accepted") {
    val table = Table(otherSchema, otherRecords)
    checkTable(table, otherSchema, otherRecords)
  }

  test("Passing only one illegal Record should result in an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      Table(schema, badRecords)
    }
  }

  test("Wrong schema should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      Table(schema, otherRecords)
    }
  }

  test("Superset of schema for records should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      Table(superSchema, testRecords)
    }
  }

  test("Subset of schema for records should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      Table(subSchema, testRecords)
    }
  }
}
*/
