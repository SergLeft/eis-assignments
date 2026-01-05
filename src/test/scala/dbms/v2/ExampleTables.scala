package dbms.v2

import dbms.v2.store.*
import dbms.v2.misc.*
import dbms.v2.run.*

trait ExampleTables {

  protected def checkRecords(table: Table, expected: Seq[Map[String, Variant]]): Unit = {
    val actual = table.map(r => r.toMap)
    assert(actual.size == table.size)
    val actualRowCounts = actual.groupBy(identity).view.mapValues(m => m.size).toMap
    val expectedRowCounts = expected.groupBy(identity).view.mapValues(m => m.size).toMap
    println(actualRowCounts)
    println()
    println(expectedRowCounts)
    assert(actualRowCounts == expectedRowCounts)
  }

  protected val studentsAttributes: Seq[String] = Seq("matriculationNumber", "firstName", "matriculationYear")
  protected val studentsTypes: Seq[DBType] = Seq(DBType.Long, DBType.String, DBType.Long)
  protected val studentsSchema: Schema = Schema(studentsAttributes.zip(studentsTypes))
  protected val studentsRecords: Seq[TableRecord] = Seq(
    TableRecord(Seq(
      "matriculationNumber" -> Variant(133676),
      "firstName" -> Variant("Maya"),
      "matriculationYear" -> Variant(2022),
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(133700),
      "firstName" -> Variant("Frank"),
      "matriculationYear" -> Variant(2022),
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(132699),
      "firstName" -> Variant("Markus"),
      "matriculationYear" -> Variant(2023),
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(105863),
      "firstName" -> Variant("Emily"),
      "matriculationYear" -> Variant(2014),
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(133899),
      "firstName" -> Variant("Boris"),
      "matriculationYear" -> Variant(2020),
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(134811),
      "firstName" -> Variant("Alex"),
      "matriculationYear" -> Variant(2023),
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(141592),
      "firstName" -> Variant("Katja"),
      "matriculationYear" -> Variant(2017),
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(121569),
      "firstName" -> Variant("Tobias"),
      "matriculationYear" -> Variant(2023),
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(156321),
      "firstName" -> Variant("Mathias"),
      "matriculationYear" -> Variant(2025),
    ))
  )

  protected val students: Table = Table(studentsSchema, studentsRecords)
  protected val slicedStudents: Table = Table(studentsSchema, studentsRecords.slice(0, 3)) // subset
  protected val otherStudentsSchema: Schema = Schema(Seq("lastName" -> DBType.String, "matriculationNumber2" -> DBType.Long, "examsWritten" -> DBType.Long))
  protected val otherStudentsRecords: Seq[TableRecord] = Seq(
    TableRecord(Seq(
      "lastName" -> Variant("Schmidt"),
      "matriculationNumber2" -> Variant(1),
      "examsWritten" -> Variant(2)
    )),
    TableRecord(Seq(
      "lastName" -> Variant("Wagner"),
      "matriculationNumber2" -> Variant(3),
      "examsWritten" -> Variant(0)
    )),
  ) 
  protected val otherStudents: Table = Table(otherStudentsSchema, otherStudentsRecords)
  protected val otherStudents2: Table = Table(otherStudentsSchema, otherStudentsRecords.slice(0, 1)) // subset

  protected val examsAttributes: Seq[String] = Seq("examDate", "matriculationNumber", "examSubject", "examGrade")
  protected val examsTypes: Seq[DBType] = Seq(DBType.String, DBType.Long, DBType.String, DBType.Double)
  protected val examsSchema: Schema = Schema(examsAttributes.zip(examsTypes))
  protected val examsRecords: Seq[TableRecord] = Seq(
    TableRecord(Seq(
      "examDate" -> Variant("2023-04-17"),
      "matriculationNumber" -> Variant(133676),
      "examSubject" -> Variant("EIS"),
      "examGrade" -> Variant(1.3)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-04-17"),
      "matriculationNumber" -> Variant(133700),
      "examSubject" -> Variant("EIS"),
      "examGrade" -> Variant(5.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-04-17"),
      "matriculationNumber" -> Variant(132699),
      "examSubject" -> Variant("EIS"),
      "examGrade" -> Variant(1.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-04-17"),
      "matriculationNumber" -> Variant(141592),
      "examSubject" -> Variant("EIS"),
      "examGrade" -> Variant(1.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-04-17"),
      "matriculationNumber" -> Variant(121569),
      "examSubject" -> Variant("PS"),
      "examGrade" -> Variant(1.7)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-06-06"),
      "matriculationNumber" -> Variant(133899),
      "examSubject" -> Variant("DSEA"),
      "examGrade" -> Variant(3.3)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-06-06"),
      "matriculationNumber" -> Variant(134811),
      "examSubject" -> Variant("DSEA"),
      "examGrade" -> Variant(4.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-06-06"),
      "matriculationNumber" -> Variant(132699),
      "examSubject" -> Variant("DSEA"),
      "examGrade" -> Variant(1.3)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-06-06"),
      "matriculationNumber" -> Variant(121569),
      "examSubject" -> Variant("DSEA"),
      "examGrade" -> Variant(1.3)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-06-09"),
      "matriculationNumber" -> Variant(132699),
      "examSubject" -> Variant("KT"),
      "examGrade" -> Variant(5.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-06-12"),
      "matriculationNumber" -> Variant(105863),
      "examSubject" -> Variant("EIP"),
      "examGrade" -> Variant(1.7)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-06-12"),
      "matriculationNumber" -> Variant(121569),
      "examSubject" -> Variant("EIP"),
      "examGrade" -> Variant(2.7)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-06-13"),
      "matriculationNumber" -> Variant(134811),
      "examSubject" -> Variant("DB1"),
      "examGrade" -> Variant(1.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-08-08"),
      "matriculationNumber" -> Variant(151503),
      "examSubject" -> Variant("ML"),
      "examGrade" -> Variant(2.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-08-08"),
      "matriculationNumber" -> Variant(151503),
      "examSubject" -> Variant("ML"),
      "examGrade" -> Variant(2.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-08-08"),
      "matriculationNumber" -> Variant(143587),
      "examSubject" -> Variant("ML"),
      "examGrade" -> Variant(3.7)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2023-09-23"),
      "matriculationNumber" -> Variant(132699),
      "examSubject" -> Variant("KT"),
      "examGrade" -> Variant(5.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2024-01-14"),
      "matriculationNumber" -> Variant(132699),
      "examSubject" -> Variant("KT"),
      "examGrade" -> Variant(2.3)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2024-01-14"),
      "matriculationNumber" -> Variant(133676),
      "examSubject" -> Variant("KT"),
      "examGrade" -> Variant(2.7)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2024-02-13"),
      "matriculationNumber" -> Variant(133700),
      "examSubject" -> Variant("EIS"),
      "examGrade" -> Variant(2.0)
    )),
    TableRecord(Seq(
      "examDate" -> Variant("2024-02-13"),
      "matriculationNumber" -> Variant(121569),
      "examSubject" -> Variant("EIS"),
      "examGrade" -> Variant(1.0)
    ))
  )

  protected val profAttributes: Seq[String] = Seq("name", "examSubject")
  protected val profTypes: Seq[DBType] = Seq(DBType.String, DBType.String)
  protected val profSchema: Schema = Schema(profAttributes.zip(profTypes))
  protected val profRecords: Seq[TableRecord] = Seq(
    TableRecord(Seq(
      "name" -> Variant("Blumenstock"),
      "examSubject" -> Variant("DSEA")
    )),
    TableRecord(Seq(
      "name" -> Variant("Blumenstock"),
      "examSubject" -> Variant("KT")
    )),
    TableRecord(Seq(
      "name" -> Variant("Schoemer"),
      "examSubject" -> Variant("DSEA")
    )),
    TableRecord(Seq(
      "name" -> Variant("Schoemer"),
      "examSubject" -> Variant("KT")
    )),
    TableRecord(Seq(
      "name" -> Variant("Schoemer"),
      "examSubject" -> Variant("EIP")
    )),
    TableRecord(Seq(
      "name" -> Variant("Erdweg"),
      "examSubject" -> Variant("PS")
    )),
    TableRecord(Seq(
      "name" -> Variant("Erdweg"),
      "examSubject" -> Variant("EIS")
    )),
    TableRecord(Seq(
      "name" -> Variant("Kramer"),
      "examSubject" -> Variant("ML")
    )),
    TableRecord(Seq(
      "name" -> Variant("Bouros"),
      "examSubject" -> Variant("DB1")
    ))
  )
  protected val profs: Table = Table(profSchema, profRecords)
  protected val slicedProfs: Table = Table(profSchema, profRecords.slice(2, 6))
  
  protected val exams: Table = Table(examsSchema, examsRecords)
  protected val slicedExams: Table = Table(examsSchema, examsRecords.slice(0, 10)) // subset
  protected val otherExamsSchema: Schema = Schema(Seq("examTime" -> DBType.String, "matriculationNumber2" -> DBType.Long, "examSubject" -> DBType.String))
  protected val otherExamsRecords: Seq[TableRecord] = Seq(
    TableRecord(Seq(
      "examTime" -> Variant("12:30"),
      "matriculationNumber2" -> Variant(1),
      "examSubject" -> Variant("EIS")
    )),
    TableRecord(Seq(
      "examTime" -> Variant("09:15"),
      "matriculationNumber2" -> Variant(1),
      "examSubject" -> Variant("KT")
    )),
    TableRecord(Seq(
      "examTime" -> Variant("12:30"),
      "matriculationNumber2" -> Variant(1),
      "examSubject" -> Variant("PS")
    )),
    TableRecord(Seq(
      "examTime" -> Variant("06:15"),
      "matriculationNumber2" -> Variant(0),
      "examSubject" -> Variant("KT 2")
    ))
  ) 
  protected val otherExams: Table = Table(otherExamsSchema, otherExamsRecords)
  protected val otherExams2: Table = Table(otherExamsSchema, otherExamsRecords.slice(1, 2)) // subset

  protected val paymentAttributes: Seq[String] = Seq("matriculationNumber", "payedAmount")
  protected val paymentTypes: Seq[DBType] = Seq(DBType.Long, DBType.Double)
  protected val paymentSchema: Schema = Schema(paymentAttributes.zip(paymentTypes))
  protected val paymentRecords: Seq[TableRecord] = Seq(
    TableRecord(Seq(
      "matriculationNumber" -> Variant(133676),
      "payedAmount" -> Variant(651.31)
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(133700),
      "payedAmount" -> Variant(341.71)
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(132699),
      "payedAmount" -> Variant(651.31)
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(105863),
      "payedAmount" -> Variant(651.31)
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(133899),
      "payedAmount" -> Variant(1024.24)
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(134811),
      "payedAmount" -> Variant(341.71)
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(141592),
      "payedAmount" -> Variant(341.71)
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(121569),
      "payedAmount" -> Variant(1024.24)
    )),
    TableRecord(Seq(
      "matriculationNumber" -> Variant(156321),
      "payedAmount" -> Variant(651.31)
    ))
  )

  protected val payment: Table = Table(paymentSchema, paymentRecords)
  protected val slicedPayment: Table = Table(paymentSchema, payment.slice(0, 3))
}
