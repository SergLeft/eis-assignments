package dbms.v1

import org.scalatest.funsuite.AnyFunSuite

class ScoredProjectionSuite extends AnyFunSuite {

  private val schema = Seq("empId", "salary", "bonus")
  private val subSchema = Seq("empId", "salary")

  private val testRecords = Seq(
    TableRecord(schema.zip(Seq(3.0, 3033.4, 0.4))),
    TableRecord(schema.zip(Seq(4.0, 1235.3, 7.1))),
    TableRecord(schema.zip(Seq(5.0, 2943.7, 0.0))),
    TableRecord(schema.zip(Seq(7.0, 845.8, 0.1))),
    TableRecord(schema.zip(Seq(11.0, 1235.3, 2.1))),
    TableRecord(schema.zip(Seq(12.0, 1235.3, 7.1))),
    TableRecord(schema.zip(Seq(13.0, 1235.3, 7.1)))
  )

  private def createFilledTable: Table = {
    val table = Table(schema)
    testRecords.foreach(record => table.appendRecord(record))
    table
  }

  private def extractAttribute(records: Seq[TableRecord], attribute: String): Seq[Value] = {
    records.map(record => record.getValue(attribute))
  }

  private def validateProjection(projectedSchema: Seq[String], old: Table, projected: Table): Unit = {
    assert(old.schema == schema)
    assert(projected.schema == projectedSchema)
    assert(projected.numRecords == old.numRecords)

    val projectedRecords = (0 until projected.numRecords).map(i => projected.getRecord(i))
    val oldRecords = (0 until old.numRecords).map(i => old.getRecord(i))

    projectedSchema.foreach { attribute =>
      assert(extractAttribute(oldRecords, attribute) == extractAttribute(projectedRecords, attribute))
    }
  }

  test("Empty output attributes should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      createFilledTable.project(Seq.empty)
    }
  }

  test("Projecting to an unknown attribute should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      createFilledTable.project(Seq("none"))
    }
  }

  test("Projecting to a proper superset of the schema should throw an IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      createFilledTable.project(schema ++ Seq("hours"))
    }
  }

  test("Projecting to the same schema should also work") {
    val filledTable = createFilledTable
    val projected = filledTable.project(schema)
    validateProjection(schema, filledTable, projected)
  }

  test("Projecting to the same schema should still create a copy") {
    val filledTable = createFilledTable
    val projected = filledTable.project(schema)
    projected.appendRecord(Seq("empId" -> 100.0, "salary" -> 100.0, "bonus" -> 100.0))
    assert(filledTable.numRecords == 7)
    assert(projected.numRecords == 8)
  }

  test("Projecting to a subset of the schema should copy only the required values") {
    val filledTable = createFilledTable
    val projected = filledTable.project(subSchema)
    validateProjection(subSchema, filledTable, projected)
  }

  test("Projecting an empty table should still yield an empty table") {
    val filledTable = Table(schema)
    val projected = filledTable.project(Seq("salary"))
    validateProjection(Seq("salary"), filledTable, projected)
  }
}
