package dbms.v1

import org.scalatest.funsuite.AnyFunSuite

class ScoredEmptyTableSuite extends AnyFunSuite {

  private val schema = Seq("empId", "salary", "bonus")

  test("Empty record should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      TableRecord(Seq.empty)
    }
  }

  test("Empty schema should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      Table(Seq.empty)
    }
  }

  test("Append empty record should throw an IllegalArgumentException") {
    val table = Table(schema)
    assertThrows[IllegalArgumentException] {
      table.appendRecord(Seq.empty)
    }
  }

  test("A table with zero records should be valid") {
    val table = Table(schema)
    assert(table.schema == schema)
    assert(table.numRecords == 0)
  }
}
