package dbms.v2

import dbms.v2.store.*
import dbms.v2.misc.*
import dbms.v2.run.*
import org.scalatest.funsuite.AnyFunSuite


class ScoredQuerySuite extends AnyFunSuite, ExampleTables {

  // if you want to see all records of the exams/students table, you can look it up in ExampleTables.scala

  test("knownExams: Should return an empty table when students and exams are empty") {
    val result = knownExams(Table(studentsSchema), Table(examsSchema))
    assert(result.isEmpty)
  }

  test("knownExams: The resulting table should have the correct schema") {
    val result = knownExams(Table(studentsSchema), Table(examsSchema))
    assert(result.schema.attributes == Set("examDate", "examSubject"))
  }

  test("knownExams should not show an exam multiple times") {
    val expected = Seq(
      Map("examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examDate" -> Variant("2023-06-12"), "examSubject" -> Variant("EIP")),
      Map("examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("PS")),
      Map("examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT")),
      Map("examDate" -> Variant("2023-09-23"), "examSubject" -> Variant("KT")),
      Map("examDate" -> Variant("2024-01-14"), "examSubject" -> Variant("KT")),
      Map("examDate" -> Variant("2023-06-13"), "examSubject" -> Variant("DB1")),
      Map("examDate" -> Variant("2023-08-08"), "examSubject" -> Variant("ML")),
      Map("examDate" -> Variant("2024-02-13"), "examSubject" -> Variant("EIS"))
    )
    val result = knownExams(students, exams)
    checkRecords(result, expected)

    val expected2 = Seq(
      Map("examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("EIS")),
      Map("examDate" -> Variant("2023-04-17"), "examSubject" -> Variant("PS")),
      Map("examDate" -> Variant("2023-06-06"), "examSubject" -> Variant("DSEA")),
      Map("examDate" -> Variant("2023-06-09"), "examSubject" -> Variant("KT"))
    )
    val result2 = knownExams(slicedStudents, slicedExams)
    checkRecords(result2, expected2)
  }

  test("topStudents: Should return an empty table when students and exams are empty") {
    val result = topStudents(Table(studentsSchema), Table(examsSchema))
    assert(result.isEmpty)
  }

  test("topStudents: The resulting table should have the correct schema") {
    val result = topStudents(Table(studentsSchema), Table(examsSchema))
    assert(result.schema.attributes == Set("firstName"))
  }

  test("topStudents: Should not display students who have not participated in any EIS exam or have not received a 1.0") {
    val expected = Seq(
      Map("firstName" -> Variant("Markus")),
      Map("firstName" -> Variant("Katja")), 
      Map("firstName" -> Variant("Tobias"))
    )
    val result = topStudents(students, exams)
    checkRecords(result, expected)

    val expected2 = Seq(Map("firstName" -> Variant("Markus")))
    val result2 = topStudents(slicedStudents, slicedExams)
    checkRecords(result2, expected2)
  }

   test("earlyBirds: Should return an empty table when students and exams are empty") {
    val result = earlyBirds(Table(studentsSchema), Table(examsSchema))
    assert(result.isEmpty)
  }

  test("earlyBirds: The resulting table should have the correct schema") {
    val result = earlyBirds(Table(studentsSchema), Table(examsSchema))
    assert(result.schema.attributes == Set("firstName"))
  }

  test("earlyBirds: Should only consider students who have written DSEA and matriculated no earlier than 2022") {
    val expected = Seq(
      Map("firstName" -> Variant("Markus")),
      Map("firstName" -> Variant("Alex")),
      Map("firstName" -> Variant("Tobias"))
    )
    val result = earlyBirds(students, exams)
    checkRecords(result, expected)

    val expected2 = Seq(Map("firstName" -> Variant("Markus")))
    val result2 = earlyBirds(slicedStudents, slicedExams)
    checkRecords(result2, expected2)
  }

  test("earlyBirds: Should ignore students who did not pass the exam even though they were matriculated in 2022.") {
    val expected = Seq(
      Map("firstName" -> Variant("Markus")),
      Map("firstName" -> Variant("Alex")),
      Map("firstName" -> Variant("Tobias"))
    )
    val record = TableRecord(Seq(
      "examDate" -> Variant("2023-06-06"),
      "matriculationNumber" -> Variant(133700),
      "examSubject" -> Variant("DSEA"),
      "examGrade" -> Variant(5.0)
    ))
    exams.appendRecord(record)
    val result = earlyBirds(students, exams)
    checkRecords(result, expected)

    val expected2 = Seq(Map("firstName" -> Variant("Markus")))
    val result2 = earlyBirds(slicedStudents, slicedExams)
    slicedExams.appendRecord(record)
    checkRecords(result2, expected2)
  }

  test("personalTranscript: Should return an empty table when students and exams are empty") {
    val result = personalTranscript(Table(studentsSchema), Table(examsSchema), "KeinName")
    assert(result.isEmpty)
  }

  test("personalTranscript: The resulting table should have the correct schema") {
    val result = personalTranscript(Table(studentsSchema), Table(examsSchema), "KeinName")
    assert(result.schema.attributes == Set("examGrade", "examSubject"))
  }

  test("personalTranscript: Should show all exams(with duplicates) the student has written") {
    val expected = Seq(
      Map("examSubject" -> Variant("EIS"),  "examGrade" -> Variant(1.0)),
      Map("examSubject" -> Variant("DSEA"), "examGrade" -> Variant(1.3)), 
      Map("examSubject" -> Variant("KT"),   "examGrade" -> Variant(5.0)),
      Map("examSubject" -> Variant("KT"),   "examGrade" -> Variant(5.0)),
      Map("examSubject" -> Variant("KT"),   "examGrade" -> Variant(2.3)), 
    )
    val result = personalTranscript(students, exams, "Markus")
    checkRecords(result, expected)

    val expected2 = Seq(
      Map("examSubject" -> Variant("EIS"),  "examGrade" -> Variant(1.0)),
      Map("examSubject" -> Variant("DSEA"), "examGrade" -> Variant(1.3)), 
      Map("examSubject" -> Variant("KT"),   "examGrade" -> Variant(5.0)),
    )
    val result2 = personalTranscript(slicedStudents, slicedExams, "Markus")
    checkRecords(result2, expected2)
  }

  test("personalTranscript: Should show all exams(with duplicates) another student has written") {
    val expected = Seq(
      Map("examSubject" -> Variant("EIS"), "examGrade" -> Variant(1.3)),
      Map("examSubject" -> Variant("KT"),  "examGrade" -> Variant(2.7))
    )
    val result = personalTranscript(students, exams, "Maya")
    checkRecords(result, expected)

    val expected2 = Seq(Map("examSubject" -> Variant("EIS"), "examGrade" -> Variant(1.3)))
    val result2 = personalTranscript(slicedStudents, slicedExams, "Maya")
    checkRecords(result2, expected2)
  }
}
