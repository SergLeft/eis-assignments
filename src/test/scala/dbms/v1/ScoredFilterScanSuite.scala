package dbms.v1

import org.scalatest.funsuite.AnyFunSuite

class ScoredFilterScanSuite extends AnyFunSuite {

  private def validate(table: Table, targetAttribute: String, expected: Seq[Value]): Unit = {
    assert(table.schema == Seq(targetAttribute))
    assert(table.numRecords == expected.size)

    val tableRecords: Seq[Value] = (0 until table.numRecords).map(i => table.getRecord(i).getValue(targetAttribute))

    assert(expected == tableRecords)
  }

  private val schema = Seq("empId", "salary", "bonus")

  private val testRecords = Seq(
    TableRecord(schema.zip(Seq(3.0, 3033.4, 0.4))),
    TableRecord(schema.zip(Seq(4.0, 1235.3, 7.1))),
    TableRecord(schema.zip(Seq(5.0, 2943.7, 0.0))),
    TableRecord(schema.zip(Seq(7.0, 845.8, 0.1))),
    TableRecord(schema.zip(Seq(11.0, 1235.3, 2.1))),
    TableRecord(schema.zip(Seq(12.0, 1235.3, 7.1))),
    TableRecord(schema.zip(Seq(13.0, 1235.3, 7.1)))
  )

  private def filledTable: Table = {
    val table = Table(schema)
    testRecords.foreach(record => table.appendRecord(record))
    table
  }

  test("Unknown output attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      filledTable.filterByScan("Unknown", "salary", 1235.3)
    }
  }

  test("Unknown selection attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      filledTable.filterByScan("empId", "Unknown", 1235.3)
    }
  }

  test("Filtering for empId=-1 should return an empty table") {
    val filtered = filledTable.filterByScan("empId", "empId", -1.0)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for empId=7 should return exactly one employee") {
    val filtered = filledTable.filterByScan("empId", "empId", 7.0)
    validate(filtered, "empId", Seq(7.0))
  }

  test("Filtering for salary=1235.3 should return four employees") {
    val filtered = filledTable.filterByScan("empId", "salary", 1235.3)
    validate(filtered, "empId", Seq(4.0, 11.0, 12.0, 13.0))
  }

  test("Filtering range by unknown output attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      filledTable.filterRangeByScan("Unknown", "salary", 0.0, 10000)
    }
  }

  test("Filtering range by unknown selection attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      filledTable.filterRangeByScan("empId", "Unknown", 0.0, 10000)
    }
  }

  test("Filtering for empId between -2 and -1 should return an empty table") {
    val filtered = filledTable.filterRangeByScan("empId", "empId", -2.0, -1.0)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for empId with empty range should return an empty table") {
    val filtered = filledTable.filterRangeByScan("empId", "empId", 7.0, 6.0)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for salary with empty range should return an empty table") {
    val filtered = filledTable.filterRangeByScan("empId", "salary", 845.8, 845.8)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for empId between 6.5 and 7.5 should return exactly one employee") {
    val filtered = filledTable.filterRangeByScan("empId", "empId", 6.5, 7.5)
    validate(filtered, "empId", Seq(7.0))
  }

  test("Filtering for empId of at least 5.0 should return 5 employees") {
    val filtered = filledTable.filterRangeByScan("empId", "empId", 5.0, Double.PositiveInfinity)
    validate(filtered, "empId", Seq(5.0, 7.0, 11.0, 12.0, 13.0))
  }

  test("Filtering for empId of at most 8.0 should return 4 employees") {
    val filtered = filledTable.filterRangeByScan("empId", "empId", Double.NegativeInfinity, 8.0)
    validate(filtered, "empId", Seq(3.0, 4.0, 5.0, 7.0))
  }

  test("Filtering with no bounds should return all employees") {
    val filtered = filledTable.filterRangeByScan("empId", "empId", Double.NegativeInfinity, Double.PositiveInfinity)
    validate(filtered, "empId", Seq(3.0, 4.0, 5.0, 7.0, 11.0, 12.0, 13.0))
  }
}