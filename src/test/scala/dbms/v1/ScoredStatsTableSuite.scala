package dbms.v1

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.funsuite.AnyFunSuite


class ScoredStatsTableSuite extends AnyFunSuite {

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

  private def emptyTable = StatsTable(schema)

  private def createFilledTable: StatsTable = {
    val table = StatsTable(schema)
    testRecords.foreach(record => table.appendRecord(record))
    table
  }

  test("Calling minimum on empty StatsTable should throw NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      emptyTable.minimum("empId")
    }
  }

  test("Calling maximum on empty StatsTable should throw NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      emptyTable.maximum("empId")
    }
  }

  test("Calling minimum on attribute which is not in StatsTable should throw IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      emptyTable.minimum("hours")
    }
  }

  test("Calling maximum on attribute which is not in StatsTable should throw IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      emptyTable.maximum("hours")
    }
  }

  test("Calling minimum for salary on test records should return 845.8") {
    assert(createFilledTable.minimum("salary") == 845.8)
  }

  test("Calling maximum for empId on test records should return 13") {
    assert(createFilledTable.maximum("empId") == 13)
  }

  test("Inserting new minimum for salary should return new minimum") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(14.0, 743.2, 0.5)))
    assert(table.minimum("salary") == 743.2)
  }

  test("Inserting new maximum for bonus should return new maximum") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(14.0, 743.2, 13.3)))
    assert(table.maximum("bonus") == 13.3)
  }

  test("Inserting no new minimum for salary should return old minimum") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(14.0, 957.2, 0.4)))
    assert(table.minimum("salary") == 845.8)
  }

  test("Inserting no new maximum for empId should return old minimum") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(9.0, 1327.5, 0.6)))
    assert(table.maximum("empId") == 13)
  }
}