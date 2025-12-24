package dbms.v1

import org.scalatest.funsuite.AnyFunSuite

class ScoredStatsTableRegressionSuite extends AnyFunSuite {

  // each test in here only counts for 0.5 points

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

  private def createFilledTable: StatsTable = {
    val table = StatsTable(schema)
    testRecords.foreach(record => table.appendRecord(record))
    table
  }

  private def emptyTable: StatsTable = StatsTable(schema)

  test("Unknown output attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      createFilledTable.filterByScan("Unknown", "salary", 1235.3)
    }
  }

  test("Unknown selection attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      createFilledTable.filterByScan("empId", "Unknown", 1235.3)
    }
  }

  test("Filtering on an empty table should return an empty table") {
    val filtered = emptyTable.filterByScan("empId", "salary", 3400)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering by range on an empty table should return an empty table") {
    val filtered = emptyTable.filterRangeByScan("empId", "bonus", 0.0, 0.5)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for salary bigger than 3400 should return empty table") {
    val filtered = createFilledTable.filterByScan("empId", "salary", 3400)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for salary lower than 845.8 should return empty table") {
    val filtered = createFilledTable.filterByScan("empId", "salary", 800)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for empId=7 should return exactly one employee") {
    val filtered = createFilledTable.filterByScan("empId", "empId", 7.0)
    validate(filtered, "empId", Seq(7.0))
  }

  test("Filtering for salary=1235.3 should return four employees") {
    val filtered = createFilledTable.filterByScan("empId", "salary", 1235.3)
    validate(filtered, "empId", Seq(4.0, 11.0, 12.0, 13.0))
  }

  test("Filtering range by unknown output attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      createFilledTable.filterRangeByScan("Unknown", "salary", 0.0, 10000)
    }
  }

  test("Filtering range by unknown selection attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      createFilledTable.filterRangeByScan("empId", "Unknown", 0.0, 10000)
    }
  }

  test("Filtering for bonus bigger than 0.7 should return empty table") {
    val filtered = createFilledTable.filterRangeByScan("empId", "bonus", 0.8, 1.5)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for salary between 0 and 800 should return empty table") {
    val filtered = createFilledTable.filterRangeByScan("empId", "salary", 0, 800)
    assert(filtered.schema == Seq("empId"))
    assert(filtered.numRecords == 0)
  }

  test("Filtering for bonus between 0.0 and 0.5 should return 3 employees") {
    val filtered = createFilledTable.filterRangeByScan("empId", "bonus", 0.0, 0.5)
    assert(filtered.schema == Seq("empId"))
    validate(filtered, "empId", Seq(3.0, 5.0, 7.0))
  }

  test("Filtering for salaries between 800 and 3033.5 should return all employees") {
    val filtered = createFilledTable.filterRangeByScan("empId", "salary", 800, 3033.5)
    assert(filtered.schema == Seq("empId"))
    validate(filtered, "empId", Seq(3.0, 4.0, 5.0, 7.0, 11.0, 12.0, 13.0))
  }

  test("Filtering for empId between 4 and 7 should return 2 employees") {
    val filtered = createFilledTable.filterRangeByScan("empId", "empId", 4, 7)
    assert(filtered.schema == Seq("empId"))
    validate(filtered, "empId", Seq(4.0, 5.0))
  }

  test("Filtering without bounds should return all employees") {
    val filtered = createFilledTable.filterRangeByScan("empId", "salary", Double.NegativeInfinity, Double.PositiveInfinity)
    assert(filtered.schema == Seq("empId"))
    validate(filtered, "empId", Seq(3.0, 4.0, 5.0, 7.0, 11.0, 12.0, 13.0))
  }
}
