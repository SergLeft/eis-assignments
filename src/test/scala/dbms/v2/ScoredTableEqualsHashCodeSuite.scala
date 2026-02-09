package dbms.v2

import dbms.v2.store.*
import dbms.v2.misc.*

import org.scalatest.funsuite.AnyFunSuite


class ScoredTableEqualsHashCodeSuite extends AnyFunSuite {

  private val studentsSchema = Schema(Seq("id" -> DBType.Long, "grade" -> DBType.Double, "bonus" -> DBType.Double))
  private val equivalentSchema = Schema(Seq("grade" -> DBType.Double, "bonus" -> DBType.Double, "id" -> DBType.Long))

  private val students = Table(studentsSchema)
  students.appendRecord(Seq("id" -> Variant(42657), "grade" -> Variant(1.3), "bonus" -> Variant(0.0)))
  students.appendRecord(Seq("id" -> Variant(78524), "grade" -> Variant(1.0), "bonus" -> Variant(0.3)))
  students.appendRecord(Seq("id" -> Variant(66666), "grade" -> Variant(3.7), "bonus" -> Variant(0.3)))
  students.appendRecord(Seq("id" -> Variant(42342), "grade" -> Variant(2.7), "bonus" -> Variant(0.7)))
  students.createIndex("id", IndexType.TreeIndex)
  students.createIndex("grade", IndexType.TreeIndex)
  students.createIndex("bonus", IndexType.TreeIndex)

  private val studentsCopy = Table(studentsSchema)
  studentsCopy.appendRecord(Seq("grade" -> Variant(1.3), "id" -> Variant(42657), "bonus" -> Variant(0.0)))
  studentsCopy.appendRecord(Seq("grade" -> Variant(1.0), "id" -> Variant(78524), "bonus" -> Variant(0.3)))
  studentsCopy.appendRecord(Seq("grade" -> Variant(3.7), "id" -> Variant(66666), "bonus" -> Variant(0.3)))
  studentsCopy.appendRecord(Seq("grade" -> Variant(2.7), "id" -> Variant(42342), "bonus" -> Variant(0.7)))

  private val studentsCopy2 = Table(equivalentSchema)
  studentsCopy2.appendRecord(Seq("grade" -> Variant(1.3), "id" -> Variant(42657), "bonus" -> Variant(0.0)))
  studentsCopy2.appendRecord(Seq("grade" -> Variant(1.0), "id" -> Variant(78524), "bonus" -> Variant(0.3)))
  studentsCopy2.appendRecord(Seq("grade" -> Variant(3.7), "id" -> Variant(66666), "bonus" -> Variant(0.3)))
  studentsCopy2.appendRecord(Seq("grade" -> Variant(2.7), "id" -> Variant(42342), "bonus" -> Variant(0.7)))

  private val studentsShuffle = Table(studentsSchema)
  studentsShuffle.appendRecord(Seq("grade" -> Variant(2.7), "id" -> Variant(42342), "bonus" -> Variant(0.7)))
  studentsShuffle.appendRecord(Seq("grade" -> Variant(1.3), "id" -> Variant(42657), "bonus" -> Variant(0.0)))
  studentsShuffle.appendRecord(Seq("grade" -> Variant(3.7), "id" -> Variant(66666), "bonus" -> Variant(0.3)))
  studentsShuffle.appendRecord(Seq("grade" -> Variant(1.0), "id" -> Variant(78524), "bonus" -> Variant(0.3)))

  private val someStudents = Table(studentsSchema)
  someStudents.appendRecord(Seq("grade" -> Variant(2.7), "id" -> Variant(42342), "bonus" -> Variant(0.7)))
  someStudents.appendRecord(Seq("grade" -> Variant(1.3), "id" -> Variant(42657), "bonus" -> Variant(0.0)))

  private val otherStudents = Table(studentsSchema)
  otherStudents.appendRecord(Seq("grade" -> Variant(3.7), "id" -> Variant(66666), "bonus" -> Variant(0.3)))
  otherStudents.appendRecord(Seq("grade" -> Variant(1.0), "id" -> Variant(78524), "bonus" -> Variant(0.3)))

  private val emptyTable = Table(studentsSchema)

  private def checkColumn(table: Table, attribute: String, expected: Seq[Variant]): Boolean = {
    val x = table.map(r => r.getValue(attribute))
    x == expected
  }

  test("a table should be equal to itself") {
    assert(students == students)
    assert(students.hashCode == students.hashCode)
  }

  test("two tables created from the same data should be equal") {
    assert(students == studentsCopy)
    assert(students.hashCode == studentsCopy.hashCode)
  }

  test("two tables created from the same data with a equivalent schema should be equal") {
    assert(students == studentsCopy2)
    assert(students.hashCode == studentsCopy2.hashCode)
  }

  test("two tables with different row order should not be equal") {
    assert(students != studentsShuffle)
  }

  test("two tables of different length should not be equal") {
    assert(students != someStudents)
  }

  test("two different tables should not be equal") {
    assert(someStudents != otherStudents)
  }

  test("an empty table should not equal a full table") {
    assert(emptyTable != students)
  }

  test("an empty table with the same schema should equal an empty table") {
    val anotherEmpty = Table(studentsSchema)
    assert(emptyTable == anotherEmpty)
    assert(emptyTable.hashCode == anotherEmpty.hashCode)
  }

  test("an empty table with a different schema size should not equal an empty table") {
    val anotherSchema = Schema(Seq("id" -> DBType.Long, "grade" -> DBType.Double))
    val anotherEmpty = Table(anotherSchema)
    assert(emptyTable != anotherEmpty)
  }

  test("another empty table with a different schema names should not equal an empty table") {
    val anotherSchema = Schema(Seq("apples" -> DBType.Double, "bananas" -> DBType.Double, "grapes" -> DBType.Double))
    val anotherEmpty = Table(anotherSchema)
    assert(emptyTable != anotherEmpty)
  }

  test("an empty table should equal an empty table with an equivalent schema") {
    val anotherEmpty = Table(equivalentSchema)
    assert(emptyTable == anotherEmpty)
    assert(emptyTable.hashCode == anotherEmpty.hashCode)
  }
}
