package dbms.v1

import org.scalatest.funsuite.AnyFunSuite

class ScoredQuerySuite extends AnyFunSuite {
  
  private val schema = Seq("studentID", "grade", "bonus")

  private def createEmptyTable: Table = {
    val table = Table(schema)
    schema.foreach(column => table.createIndex(column, IndexType.TreeIndex))
    table
  }

  def createFilledTable: Table = {
    val students = createEmptyTable
    students.appendRecord(Seq("studentID" -> 42657.0, "grade" -> 1.3, "bonus" -> 0.0))
    students.appendRecord(Seq("studentID" -> 78524.0, "grade" -> 2.0, "bonus" -> 0.0))
    students.appendRecord(Seq("studentID" -> 66666.0, "grade" -> 3.7, "bonus" -> 0.3))
    students.appendRecord(Seq("studentID" -> 42342.0, "grade" -> 2.7, "bonus" -> 0.0))
    students.appendRecord(Seq("studentID" -> 97891.0, "grade" -> 1.7, "bonus" -> 0.0))
    students.appendRecord(Seq("studentID" -> 25466.0, "grade" -> 1.7, "bonus" -> 0.7))
    students.appendRecord(Seq("studentID" -> 89134.0, "grade" -> 2.0, "bonus" -> 0.0))
    students.appendRecord(Seq("studentID" -> 12345.0, "grade" -> 3.3, "bonus" -> 0.0))
    students.appendRecord(Seq("studentID" -> 54534.0, "grade" -> 2.3, "bonus" -> 0.3))
    students.appendRecord(Seq("studentID" -> 78678.0, "grade" -> 5.0, "bonus" -> 0.0))
    students
  }

  //close call
  test("Empty table shouldn't have close calls") { 
    assert(closeCall(createEmptyTable).numRecords == 0)
  }

  test("table without close calls should be empty") {
    val original = createFilledTable
    val numRecords = original.numRecords
    assert(closeCall(original).numRecords == 0)
    assert(original.numRecords == numRecords)
  }
  
  def getStudentId(table: Table, index: Int): Double = table.getRecord(index).getValue("studentID")

  test("table with close calls should return the correct amount of records") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(31256.0, 4.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 4.0, 0.3)))
    val numRecords = table.numRecords
    val queryResult = closeCall(table)
    assert(queryResult.numRecords == 2)
    assert(getStudentId(queryResult, 0) == 31256.0)
    assert(getStudentId(queryResult, 1) == 32402.0)
    assert(table.numRecords == numRecords)
  }

  test("table with close calls should return the correct result when having one qualifying record") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(31256.0, 4.0, 0.0)))
    val numRecords = table.numRecords
    val queryResult = closeCall(table)
    assert(queryResult.numRecords == 1)
    assert(getStudentId(queryResult, 0) == 31256.0)
    assert(table.numRecords == numRecords)
  }

  test("table with only close calls should return the correct result") {
    val table = createEmptyTable
    table.appendRecord(schema.zip(Seq(31256.0, 4.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 4.0, 0.3)))
    table.appendRecord(schema.zip(Seq(34804.0, 4.0, 0.0)))
    val numRecords = table.numRecords
    val queryResult = closeCall(table)
    assert(queryResult.numRecords == 3)
    assert(getStudentId(queryResult, 0) == 31256.0)
    assert(getStudentId(queryResult, 1) == 32402.0)
    assert(getStudentId(queryResult, 2) == 34804.0)
    assert(table.numRecords == numRecords)
  }

  //passed
  test("Empty table shouldn't have passed students") {
    assert(passed(createEmptyTable).numRecords == 0)
  }

  test("table without passed students should be empty") {
    val table = createEmptyTable
    table.appendRecord(schema.zip(Seq(31256.0, 5.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 5.0, 0.3)))
    table.appendRecord(schema.zip(Seq(34804.0, 5.0, 0.0)))
    val numRecords = table.numRecords
    assert(passed(table).numRecords == 0)
    assert(table.numRecords == numRecords)
  }

  test("table with passed students should return the correct amount of records") {
    val table = createEmptyTable
    table.appendRecord(schema.zip(Seq(31256.0, 2.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 4.0, 0.3)))
    table.appendRecord(schema.zip(Seq(34804.0, 5.0, 0.7)))
    table.appendRecord(schema.zip(Seq(38608.0, 5.0, 0.3)))
    table.appendRecord(schema.zip(Seq(36206.0, 5.0, 0.0)))
    val numRecords = table.numRecords
    val queryResult = passed(table)
    assert(queryResult.numRecords == 2)
    assert(getStudentId(queryResult, 0) == 31256.0)
    assert(getStudentId(queryResult, 1) == 32402.0)
    assert(table.numRecords == numRecords)
  }

  test("table with one passed student should return the correct result when having one qualifying record") {
    val table = createEmptyTable
    table.appendRecord(schema.zip(Seq(31256.0, 2.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 5.0, 0.3)))
    table.appendRecord(schema.zip(Seq(34804.0, 5.0, 0.7)))
    table.appendRecord(schema.zip(Seq(38608.0, 5.0, 0.3)))
    table.appendRecord(schema.zip(Seq(36206.0, 5.0, 0.0)))
    val numRecords = table.numRecords
    val queryResult = passed(table)
    assert(queryResult.numRecords == 1)
    assert(getStudentId(queryResult, 0) == 31256.0)
    assert(table.numRecords == numRecords)
  }

  test("table with only passed students should return the correct result") {
    val table = createEmptyTable
    table.appendRecord(schema.zip(Seq(32402.0, 2.3, 0.3)))
    table.appendRecord(schema.zip(Seq(31256.0, 3.0, 0.0)))
    table.appendRecord(schema.zip(Seq(38608.0, 4.0, 0.3)))
    val numRecords = table.numRecords
    val queryResult = passed(table)
    assert(queryResult.numRecords == 3)
    assert(getStudentId(queryResult, 0) == 32402.0)
    assert(getStudentId(queryResult, 1) == 31256.0)
    assert(getStudentId(queryResult, 2) == 38608.0)
    assert(table.numRecords == numRecords)
  }

  //DoesStudentExists
  test("Empty table shouldn't have any students") {
    assert(!doesStudentExist(createEmptyTable, 187.0))
  }

  test("table should contain student") {
    val original = createFilledTable
    val numRecords = original.numRecords
    assert(doesStudentExist(original, 78524.0))
    assert(original.numRecords == numRecords)
  }

  test("table should not contain student") {
    val original = createFilledTable
    val numRecords = original.numRecords
    assert(!doesStudentExist(original, 78525.0))
    assert(original.numRecords == numRecords)
  }

  //BestGradeNoBonus
  test("Empty table shouldn't have any best grades without a bonus") {
    assert(countBestGradeNoBonus(createEmptyTable) == 0)
  }

  test("An table without any 1.0 should be empty") {
    val table = createFilledTable
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 0)
    assert(table.numRecords == numRecords)
  }

  test("An table with only 1.0 and no bonuses should be full") {
    val table = createEmptyTable
    table.appendRecord(schema.zip(Seq(31256.0, 1.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 1.0, 0.0)))
    table.appendRecord(schema.zip(Seq(34804.0, 1.0, 0.0)))
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 3)
    assert(table.numRecords == numRecords)
  }

  test("An table with only 1.0 and some bonuses should be counted correctly") {
    val table = createEmptyTable
    table.appendRecord(schema.zip(Seq(31256.0, 1.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 1.0, 0.3)))
    table.appendRecord(schema.zip(Seq(34804.0, 1.0, 0.0)))
    table.appendRecord(schema.zip(Seq(38608.0, 1.0, 0.3)))
    table.appendRecord(schema.zip(Seq(36206.0, 1.0, 0.0)))
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 3)
    assert(table.numRecords == numRecords)
  }
  
  test("An table with only 1.0 but all bonuses should be empty") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(31256.0, 1.0, 0.3)))
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 0)
    assert(table.numRecords == numRecords)
  }

  test("An table with only one 1.0 but with an additional bonus should be empty") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(31256.0, 1.0, 0.3)))
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 0)
    assert(table.numRecords == numRecords)
  }

  test("An table with only one 1.0 and no bonus should contain one element") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(31256.0, 1.0, 0.0)))
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 1)
    assert(table.numRecords == numRecords)
  }

  test("An table with some 1.0 which have no further bonus should be counted correctly") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(31256.0, 1.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 1.0, 0.0)))
    table.appendRecord(schema.zip(Seq(34804.0, 1.0, 0.0)))
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 3)
    assert(table.numRecords == numRecords)
  }

  test("An table with some 1.0 and some bonuses should be counted correctly") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(31256.0, 1.0, 0.0)))
    table.appendRecord(schema.zip(Seq(32402.0, 1.0, 0.3)))
    table.appendRecord(schema.zip(Seq(34804.0, 1.0, 0.0)))
    table.appendRecord(schema.zip(Seq(38608.0, 1.0, 0.3)))
    table.appendRecord(schema.zip(Seq(36206.0, 1.0, 1.0)))
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 2)
    assert(table.numRecords == numRecords)
  }

  test("An table with some 1.0 and only bonuses greater 0.0 should be counted correctly") {
    val table = createFilledTable
    table.appendRecord(schema.zip(Seq(31256.0, 1.0, 0.7)))
    table.appendRecord(schema.zip(Seq(32402.0, 1.0, 0.3)))
    table.appendRecord(schema.zip(Seq(34804.0, 1.0, 0.3)))
    table.appendRecord(schema.zip(Seq(38608.0, 1.0, 0.3)))
    table.appendRecord(schema.zip(Seq(36206.0, 1.0, 1.0)))
    val numRecords = table.numRecords
    assert(countBestGradeNoBonus(table) == 0)
    assert(table.numRecords == numRecords)
  }

}
