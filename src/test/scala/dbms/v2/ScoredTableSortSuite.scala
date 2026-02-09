package dbms.v2

import dbms.v2.misc.*
import dbms.v2.store.*
import org.scalatest.funsuite.AnyFunSuite


class ScoredTableSortSuite extends AnyFunSuite {
  private val attributes = Seq("salary", "bonus", "department", "id")
  private val types = Seq(DBType.Double, DBType.Long, DBType.String, DBType.Long)
  private val records = IndexedSeq(
    TableRecord(Seq(
      "salary" -> Variant(3870.0),
      "bonus" -> Variant(300),
      "department" -> Variant("software"),
      "id" -> Variant(0)
    )),
    TableRecord(Seq(
      "salary" -> Variant(4200.0),
      "bonus" -> Variant(500),
      "department" -> Variant("marketing"),
      "id" -> Variant(1)
    )),
    TableRecord(Seq(
      "salary" -> Variant(3600.0),
      "bonus" -> Variant(300),
      "department" -> Variant("software"),
      "id" -> Variant(2)
    )),
    TableRecord(Seq(
      "salary" -> Variant(3000.0),
      "bonus" -> Variant(900),
      "department" -> Variant("management"),
      "id" -> Variant(3)
    )),
    TableRecord(Seq(
      "salary" -> Variant(3600.0),
      "bonus" -> Variant(0),
      "department" -> Variant("distribution"),
      "id" -> Variant(4)
    ))
  )

  private def checkColumn(table: Table, attribute: String, expected: Seq[Variant]): Boolean = {
    val x = table.map(r => r.getValue(attribute))
    x == expected
  }

  private val schema: Schema = Schema(attributes.zip(types))

  test("the empty table sorted by one attribute should still be empty") {
    val emptyTable = Table(schema)
    val sortedTable = emptyTable.sortBy("department")
    assert(sortedTable.isEmpty)
  }

  test("the empty table sorted by multiple attributes should still be empty") {
    val emptyTable = Table(schema)
    val sortedTable = emptyTable.sortBy(Seq("department", "salary"))
    assert(sortedTable.isEmpty)
  }

  test(
    "sorting a table by an attribute that does not exist should result in an error"
  ) {
    val table = Table(schema, records)
    assertThrows[IllegalArgumentException] {
      table.sortBy("company")
    }
  }

  test(
    "sorting by a non-existent attribute should result in an error even if it is not used"
  ) {
    val table = Table(schema, records)
    assertThrows[IllegalArgumentException] {
      table.sortBy(Seq("id", "country"))
    }
  }

  test(
    "sorting by a non-existent attribute should result in an error even if the Table is empty"
  ) {
    val table = Table(schema)
    assertThrows[IllegalArgumentException] {
      table.sortBy(Seq("id", "country"))
    }
  }

  test(
    "a table sorted by one attribute should have its records ordered by that attribute"
  ) {
    val table = Table(schema, records)
    val sortedTable = table.sortBy("bonus")
    val expectedBonusOrder = Seq(0, 300, 300, 500, 900).map(Variant(_))
    assert(checkColumn(sortedTable, "bonus", expectedBonusOrder))
  }

  test(
    "sorting by multiple attributes should sort records by the first, then the second, and so on"
  ) {
    val table = Table(schema, records)
    val sortedTable = table.sortBy(Seq("bonus", "salary"))
    assert(checkColumn(sortedTable, "bonus", Seq(0, 300, 300, 500, 900).map(Variant(_))))
    assert(checkColumn(sortedTable, "salary", Seq(3600.0, 3600.0, 3870.0, 4200.0, 3000.0).map(Variant(_))))
    assert(checkColumn(sortedTable, "department", Seq("distribution", "software", "software", "marketing", "management").map(Variant(_))))
    assert(checkColumn(sortedTable, "id", Seq(4, 2, 0, 1, 3).map(Variant(_))))
  }

  test(
    "sorting by multiple attributes should sort records by the first, then the second, and so on (2)"
  ) {
    val table = Table(schema, records)
    val sortedTable = table.sortBy(Seq("salary", "bonus"))
    assert(checkColumn(sortedTable, "bonus", Seq(900, 0, 300, 300, 500).map(Variant(_))))
    assert(checkColumn(sortedTable, "salary", Seq(3000.0, 3600.0, 3600.0, 3870.0, 4200.0).map(Variant(_))))
    assert(checkColumn(sortedTable, "department", Seq("management", "distribution", "software", "software", "marketing").map(Variant(_))))
    assert(checkColumn(sortedTable, "id", Seq(3, 4, 2, 0, 1).map(Variant(_))))
  }

  test(
    "multiple sorting runs should be possible"
  ) {
    val table = Table(schema, records)
    val sortedTable = table.sortBy("bonus").sortBy("salary").sortBy("department")
    assert(checkColumn(sortedTable, "id", Seq(4, 3, 1, 2, 0).map(Variant(_))))
  }
}