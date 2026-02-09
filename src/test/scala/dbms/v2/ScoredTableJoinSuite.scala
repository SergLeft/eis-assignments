package dbms.v2

import dbms.v2.store.*
import dbms.v2.misc.*
import dbms.v2.run.*
import org.scalatest.funsuite.AnyFunSuite


class ScoredTableJoinSuite extends AnyFunSuite, ExampleTables {

  // if you want to see all records of the exams/students table, you can look it up in ExampleTables.scala

  def checkSymmetric(left: Table, right: Table, expected: Seq[Map[String, Variant]]): Unit = {
    val leftSided = left.naturalJoin(right)
    checkRecords(leftSided, expected)
    val rightSided = right.naturalJoin(left)
    checkRecords(rightSided, expected)
  }

  test("throw an IllegalArgumentException when the joined tables have no common attributes") {
    val schemaA = Schema(Seq(("Test", DBType.Long)))
    val schemaB = Schema(Seq(("Test2", DBType.Long)))
    assertThrows[IllegalArgumentException](
      Table(schemaA).naturalJoin(Table(schemaB))
    )
    assertThrows[IllegalArgumentException](
      Table(schemaB).naturalJoin(Table(schemaA))
    )
  }

  test("throw an IllegalArgumentException when the joined tables have more than one common attribute") {
    val schemaA = Schema(Seq("Test" -> DBType.Long, "Test2" -> DBType.Long, "Test3" -> DBType.Long))
    val schemaB = Schema(Seq("Test" -> DBType.Long, "Test2" -> DBType.Long, "Test4" -> DBType.Long))
    assertThrows[IllegalArgumentException](
      Table(schemaA).naturalJoin(Table(schemaB))
    )
    assertThrows[IllegalArgumentException](
      Table(schemaB).naturalJoin(Table(schemaA))
    )
  }

  test("natural join should throw an error if the common attribute has different data types") {
    val schema = Schema(Seq(("Test", DBType.Long)))
    val schema2 = Schema(Seq(("Test", DBType.String), ("Test2", DBType.Long)))
    assertThrows[IllegalArgumentException](Table(schema).naturalJoin(Table(schema2)))
    assertThrows[IllegalArgumentException](Table(schema2).naturalJoin(Table(schema)))
  }

  test("natural join should work on empty tables") {
    val schema = Schema(Seq(("Test", DBType.Long), ("Test3", DBType.Long)))
    val schema2 = Schema(Seq(("Test", DBType.Long), ("Test2", DBType.Long)))
    val table1 = Table(schema)
    val table2 = Table(schema2)
    checkSymmetric(table1, table2, Seq.empty)
    val leftSided = table1.naturalJoin(table2)
    assert(leftSided.isEmpty)
    assert(leftSided.schema.attributes == Set("Test", "Test2", "Test3"))
    val rightSided = table2.naturalJoin(table1)
    assert(rightSided.isEmpty)
    assert(rightSided.schema.attributes == Set("Test", "Test2", "Test3"))
  }

  test("natural join should work if either side is empty") {
    val attributes = Seq("matriculationNumber", "lastName")
    val types = Seq(DBType.Long, DBType.String)
    val schema = Schema(attributes.zip(types))
    val other = Table(schema, Seq.empty)
    val result = students.naturalJoin(other)
    checkSymmetric(students, other, Seq.empty)
    assert(result.isEmpty)
    assert(result.schema.attributes == (studentsAttributes ++ attributes).toSet)
    val result2 = other.naturalJoin(students)
    assert(result2.isEmpty)
    assert(result2.schema.attributes == (studentsAttributes ++ attributes).toSet)
  }

  test("natural join should work on tables with no common elements") {
    val attributes = Seq("matriculationNumber", "lastName")
    val types = Seq(DBType.Long, DBType.String)
    val schema = Schema(attributes.zip(types))
    val records = Seq(
      TableRecord(Seq(
        "matriculationNumber" -> Variant(166321),
        "lastName" -> Variant("Mustermann")
      )),
      TableRecord(Seq(
        "matriculationNumber" -> Variant(167431),
        "lastName" -> Variant("Musterfrau")
      ))
    )
    val other = Table(schema, records)
    checkSymmetric(students, other, Seq.empty)
    val leftSided = students.naturalJoin(other)
    assert(leftSided.isEmpty)
    assert(leftSided.schema.attributes == (studentsAttributes ++ attributes).toSet)
    val rightSided = students.naturalJoin(other)
    assert(rightSided.isEmpty)
    assert(rightSided.schema.attributes == (studentsAttributes ++ attributes).toSet)
  }

  test("the resulting table after naturalJoining exams and students should have the correct schema") {
    val expected = Set("examDate", "matriculationNumber", "examSubject", "examGrade", "firstName", "matriculationYear")
    assert(students.naturalJoin(exams).schema.attributes == expected)
    assert(exams.naturalJoin(students).schema.attributes == expected)
  }

  test("the resulting table should have the correct schema after naturalJoining exams and students with another schema") {
    val expected = Set("examTime", "matriculationNumber2", "examSubject", "lastName", "examsWritten")
    assert(otherStudents.naturalJoin(otherExams).schema.attributes == expected)
    assert(otherExams.naturalJoin(otherStudents).schema.attributes == expected)
  }

  test("natural join on one to one correspondence of tables should yield combined data") {
    val expected = Seq(
      Map("matriculationNumber" -> Variant(133676), "firstName" -> Variant("Maya"), "matriculationYear" -> Variant(2022), "payedAmount" -> Variant(651.31)),
      Map("matriculationNumber" -> Variant(133700), "firstName" -> Variant("Frank"), "matriculationYear" -> Variant(2022), "payedAmount" -> Variant(341.71)),
      Map("matriculationNumber" -> Variant(132699), "firstName" -> Variant("Markus"), "matriculationYear" -> Variant(2023), "payedAmount" -> Variant(651.31)),
      Map("matriculationNumber" -> Variant(105863), "firstName" -> Variant("Emily"), "matriculationYear" -> Variant(2014), "payedAmount" -> Variant(651.31)),
      Map("matriculationNumber" -> Variant(133899), "firstName" -> Variant("Boris"), "matriculationYear" -> Variant(2020), "payedAmount" -> Variant(1024.24)),
      Map("matriculationNumber" -> Variant(134811), "firstName" -> Variant("Alex"), "matriculationYear" -> Variant(2023), "payedAmount" -> Variant(341.71)),
      Map("matriculationNumber" -> Variant(141592), "firstName" -> Variant("Katja"), "matriculationYear" -> Variant(2017), "payedAmount" -> Variant(341.71)),
      Map("matriculationNumber" -> Variant(121569), "firstName" -> Variant("Tobias"), "matriculationYear" -> Variant(2023), "payedAmount" -> Variant(1024.24)),
      Map("matriculationNumber" -> Variant(156321), "firstName" -> Variant("Mathias"), "matriculationYear" -> Variant(2025), "payedAmount" -> Variant(651.31))
    )
    checkSymmetric(students, payment, expected)
  }

  test("natural join on sliced student table with one to one correspondence should yield reduced data") {
    val expected = Seq(
      Map("matriculationNumber" -> Variant(133676), "firstName" -> Variant("Maya"), "matriculationYear" -> Variant(2022), "payedAmount" -> Variant(651.31)),
      Map("matriculationNumber" -> Variant(133700), "firstName" -> Variant("Frank"), "matriculationYear" -> Variant(2022), "payedAmount" -> Variant(341.71)),
      Map("matriculationNumber" -> Variant(132699), "firstName" -> Variant("Markus"), "matriculationYear" -> Variant(2023), "payedAmount" -> Variant(651.31))
    )
    checkSymmetric(slicedStudents, payment, expected)
  }

  test("natural join on sliced payment table with one to one correspondence should yield reduced data") {
    val expected = Seq(
      Map("matriculationNumber" -> Variant(133676), "firstName" -> Variant("Maya"), "matriculationYear" -> Variant(2022), "payedAmount" -> Variant(651.31)),
      Map("matriculationNumber" -> Variant(133700), "firstName" -> Variant("Frank"), "matriculationYear" -> Variant(2022), "payedAmount" -> Variant(341.71)),
      Map("matriculationNumber" -> Variant(132699), "firstName" -> Variant("Markus"), "matriculationYear" -> Variant(2023), "payedAmount" -> Variant(651.31)),
    )
    checkSymmetric(students, slicedPayment, expected)
  }

  test("natural join on one to many correspondence of tables should yield to multiple entries per student") {
    val expected = Seq(
      Map("examGrade" -> Variant(1.3), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Maya"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(2.7), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Maya"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(5.0), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Frank"), "matriculationNumber" -> Variant(133700), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(2.0), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Frank"), "matriculationNumber" -> Variant(133700), "examDate" -> Variant("2024-02-13"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.3), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(5.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(5.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-09-23"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.3), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(1.7), "matriculationYear" -> Variant(2014), "firstName" -> Variant("Emily"), "matriculationNumber" -> Variant(105863), "examDate" -> Variant("2023-06-12"), "examSubject" -> Variant("EIP")),
      Map("examGrade" -> Variant(3.3), "matriculationYear" -> Variant(2020), "firstName" -> Variant("Boris"), "matriculationNumber" -> Variant(133899), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(4.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Alex"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Alex"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-13"), "examSubject" -> Variant("DB1")),
      Map("examGrade" -> Variant(1.0), "matriculationYear" -> Variant(2017), "firstName" -> Variant("Katja"), "matriculationNumber" -> Variant(141592), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.7), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Tobias"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("PS")),
      Map("examGrade" -> Variant(1.3), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Tobias"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(2.7), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Tobias"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-12"), "examSubject" -> Variant("EIP")),
      Map("examGrade" -> Variant(1.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Tobias"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2024-02-13"), "examSubject" -> Variant("EIS"))
    )
    checkSymmetric(students, exams, expected)
  }

  test("natural join on sliced student table with one to many correspondence should still yield to multiple entries per student") {
    val expected = Seq(
      Map("examGrade" -> Variant(1.3), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Maya"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(2.7), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Maya"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(5.0), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Frank"), "matriculationNumber" -> Variant(133700), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(2.0), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Frank"), "matriculationNumber" -> Variant(133700), "examDate" -> Variant("2024-02-13"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.3), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(5.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(5.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-09-23"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.3), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
    )
    checkSymmetric(slicedStudents, exams, expected)
  }

  test("natural join on sliced exams table with one to many correspondence should still yield to multiple entries per student") {
    val expected = Seq(
      Map("examGrade" -> Variant(1.3), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Maya"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(5.0), "matriculationYear" -> Variant(2022), "firstName" -> Variant("Frank"), "matriculationNumber" -> Variant(133700), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.3), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(5.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Markus"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(3.3), "matriculationYear" -> Variant(2020), "firstName" -> Variant("Boris"), "matriculationNumber" -> Variant(133899), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(4.0), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Alex"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.0), "matriculationYear" -> Variant(2017), "firstName" -> Variant("Katja"), "matriculationNumber" -> Variant(141592), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.7), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Tobias"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("PS")),
      Map("examGrade" -> Variant(1.3), "matriculationYear" -> Variant(2023), "firstName" -> Variant("Tobias"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA"))
    )
    checkSymmetric(students, slicedExams, expected)
  }

  test("natural join on many to many correspondence of tables should yield to multiple entries per subject") {
    val expected = Seq(
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(133700), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.0), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.0), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(141592), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.7), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("PS")),
      Map("examGrade" -> Variant(3.3), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(133899), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(3.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(133899), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(4.0), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(4.0), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(1.7), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(105863), "examDate" -> Variant("2023-06-12"), "examSubject" -> Variant("EIP")),
      Map("examGrade" -> Variant(2.7), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-12"), "examSubject" -> Variant("EIP")),
      Map("examGrade" -> Variant(1.0), "name" -> Variant("Bouros"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-13"), "examSubject" -> Variant("DB1")),
      Map("examGrade" -> Variant(2.0), "name" -> Variant("Kramer"), "matriculationNumber" -> Variant(151503), "examDate" -> Variant("2023-08-08"), "examSubject" -> Variant("ML")),
      Map("examGrade" -> Variant(2.0), "name" -> Variant("Kramer"), "matriculationNumber" -> Variant(151503), "examDate" -> Variant("2023-08-08"), "examSubject" -> Variant("ML")),
      Map("examGrade" -> Variant(3.7), "name" -> Variant("Kramer"), "matriculationNumber" -> Variant(143587), "examDate" -> Variant("2023-08-08"), "examSubject" -> Variant("ML")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-09-23"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-09-23"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.3), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.7), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.7), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.0), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(133700), "examDate" -> Variant("2024-02-13"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.0), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2024-02-13"), "examSubject" -> Variant("EIS"))
    )
    checkSymmetric(exams, profs, expected)
  }

  test("natural join on sliced exams table with many to many correspondence should still yield to multiple entries per subject") {
    val expected = Seq(
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(133700), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.0), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.0), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(141592), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examGrade" -> Variant(1.7), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("PS")),
      Map("examGrade" -> Variant(3.3), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(133899), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(3.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(133899), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(4.0), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(4.0), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Blumenstock"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT"))
    )
    checkSymmetric(slicedExams, profs, expected)
  }

  test("natural join on sliced prof table with many to many correspondence should still yield to multiple entries per subject") {
    val expected = Seq(
      Map("examGrade" -> Variant(1.7), "name" -> Variant("Erdweg"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("PS")),
      Map("examGrade" -> Variant(3.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(133899), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(4.0), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(134811), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(1.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(1.7), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(105863), "examDate" -> Variant("2023-06-12"), "examSubject" -> Variant("EIP")),
      Map("examGrade" -> Variant(2.7), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(121569), "examDate" -> Variant("2023-06-12"), "examSubject" -> Variant("EIP")),
      Map("examGrade" -> Variant(5.0), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2023-09-23"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.3), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(132699), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examGrade" -> Variant(2.7), "name" -> Variant("Schoemer"), "matriculationNumber" -> Variant(133676), "examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT"))
    )
    checkSymmetric(exams, slicedProfs, expected)
  }

}
