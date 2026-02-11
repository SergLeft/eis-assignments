package dbms.v2

import dbms.v2.indexing.UnbalancedTreeIndex
import dbms.v2.misc.{DBType, IndexType, Variant}
import dbms.v2.store.*
import org.scalatest.funsuite.AnyFunSuite



class ScoredUnbalancedTreeIndexSuite extends AnyFunSuite {

  private val attributes = Seq("salary", "bonus", "department")
  private val types = Seq(DBType.Double, DBType.Long, DBType.String)
  private val records = Seq(
    TableRecord(Seq(
      "salary" -> Variant(3600.0),
      "bonus" -> Variant(300),
      "department" -> Variant("software")
    )),
    TableRecord(Seq(
      "salary" -> Variant(4200.0),
      "bonus" -> Variant(500),
      "department" -> Variant("marketing")
    )),
    TableRecord(Seq(
      "salary" -> Variant(3870.0),
      "bonus" -> Variant(300),
      "department" -> Variant("software")
    )),
    TableRecord(Seq(
      "salary" -> Variant(3000.0),
      "bonus" -> Variant(900),
      "department" -> Variant("management")
    )),
    TableRecord(Seq(
      "salary" -> Variant(3600.0),
      "bonus" -> Variant(0),
      "department" -> Variant("distribution")
    ))
  )

  private val schema = new Schema(attributes.zip(types))
  
  private val treeIndexFactory = (t: Table, attr: String) => UnbalancedTreeIndex(t, attr)

  test("setting up and clearing the index should work as expected") {
    val table = Table(schema, records)
    val index = UnbalancedTreeIndex(table, "bonus")
    assert(index.get(Variant(300)).toSet == Set(0, 2))
    index.clear()
    assert(index.get(Variant(300)).isEmpty)
  }

  test("the index should return the correct data type") {
    val table = Table(schema, records)
    val bonusIndex = UnbalancedTreeIndex(table, "bonus")
    val depIndex = UnbalancedTreeIndex(table, "department")
    assert(bonusIndex.dataType == DBType.Long)
    assert(depIndex.dataType == DBType.String)
  }

  test("the index on the empty table should only yield empty sequences for each key") {
    val emptyTable = Table(schema)
    emptyTable.createIndex("bonus", treeIndexFactory)
    assert(emptyTable.filterByIndex("bonus", Variant(300)).isEmpty)
  }

  test("the index on the table with records should point to the correct records when reading it") {
    val table = Table(schema, records)
    table.createIndex("bonus", treeIndexFactory)
    val query = table.filterByIndex("bonus", Variant(300))
    assert(query.size == 2)
    val expected = table.filterByScan("bonus", Variant(300))
    assert(query.toSet == expected.toSet)
  }

  test("the index should yield an empty sequence if you filter a non-empty table for a value bigger than all contained") {
    val table = Table(schema, records)
    table.createIndex("bonus", treeIndexFactory)
    val query = table.filterByIndex("bonus", Variant(7000))
    assert(query.isEmpty)
  }

  test("the index should yield an empty sequence if you filter a non-empty table for a value smaller than all contained") {
    val table = Table(schema, records)
    table.createIndex("bonus", treeIndexFactory)
    val query = table.filterByIndex("bonus", Variant(-400))
    assert(query.isEmpty)
  }

  test("the index should work correctly when the indexing column contains strings") {
    val table = Table(schema, records)
    table.createIndex("department", treeIndexFactory)
    val query = table.filterByIndex("department", Variant("management"))
    val expected = table.filterByScan("department", Variant("management"))
    assert(query.toSet == expected.toSet)
  }

  test("the index should find records inserted after the creation of the index") {
    val table = Table(schema, records)
    table.createIndex("bonus", treeIndexFactory)
    val record = TableRecord(Seq(
      "salary" -> Variant(2600.0),
      "bonus" -> Variant(700),
      "department" -> Variant("marketing")
    ))
    table.appendRecord(record)
    val query = table.filterByIndex( "bonus", Variant(700))
    val expected = table.filterByScan("bonus", Variant(700))
    assert(query.toSet == expected.toSet)
  }
}

