package dbms.v3.main

import dbms.v3.implementation.createNewDatabase
import dbms.v3.interface.*
import dbms.v3.interface.DataType.*


def makeTables(db: Database): Unit = {

  db.createTable("students", Seq(
    "matriculationNumber" -> NumericType,
    "firstName" -> TextType,
    "matriculationYear" -> NumericType,
  ))
  db.addRow("students", Seq(Entry(133676), Entry("Maya"),  Entry(2022)))
  db.addRow("students", Seq(Entry(133700), Entry("Frank"), Entry(2021)))
  db.addRow("students", Seq(Entry(133899), Entry("Boris"), Entry(2022)))
  db.addRow("students", Seq(Entry(134811), Entry("Alex"),  Entry(2020)))
  db.addRow("students", Seq(Entry(141592), Entry("Katja"), Entry(2017)))

  db.createTable("exams", Seq(
    "examDate" -> TextType,
    "matriculationNumber" -> NumericType,
    "examSubject" -> TextType,
    "examGrade" -> NumericType,
  ))
  db.addRow("exams", Seq(Entry("2023-04-17"), Entry(133676), Entry("EIS"), Entry(13)))
  db.addRow("exams", Seq(Entry("2023-04-17"), Entry(133700), Entry("EIS"), Entry(20)))
  db.addRow("exams", Seq(Entry("2023-04-17"), Entry(133676), Entry("PS"), Entry(17)))
  db.addRow("exams", Seq(Entry("2023-06-06"), Entry(133899), Entry("DSEA"), Entry(33)))
  db.addRow("exams", Seq(Entry("2023-06-06"), Entry(134811), Entry("DSEA"), Entry(27)))
  db.addRow("exams", Seq(Entry("2023-06-13"), Entry(134811), Entry("DB1"), Entry(10)))
  db.addRow("exams", Seq(Entry("2023-08-08"), Entry(151503), Entry("ML"), Entry(20)))

  db.createTable("gradeTitles", Seq(
    "grade" -> NumericType,
    "title" -> TextType,
  ))
  db.addRow("gradeTitles", Seq(Entry(10), Entry("sehr gut (1.0)")))
  db.addRow("gradeTitles", Seq(Entry(13), Entry("sehr gut (1.3)")))
  db.addRow("gradeTitles", Seq(Entry(17), Entry("gut (1.7)")))
  db.addRow("gradeTitles", Seq(Entry(20), Entry("gut (2.0)")))
  db.addRow("gradeTitles", Seq(Entry(23), Entry("gut (2.3)")))
  db.addRow("gradeTitles", Seq(Entry(27), Entry("befriedigend (2.7)")))
  db.addRow("gradeTitles", Seq(Entry(30), Entry("befriedigend (3.0)")))
  db.addRow("gradeTitles", Seq(Entry(33), Entry("befriedigend (3.3)")))
  db.addRow("gradeTitles", Seq(Entry(37), Entry("ausreichend (3.7)")))
  db.addRow("gradeTitles", Seq(Entry(40), Entry("ausreichend (4.0)")))
  db.addRow("gradeTitles", Seq(Entry(50), Entry("nicht bestanden")))
}


def runKnownExamsQuery(db: Database): Unit = {
  db.deleteTable("knownExams")
  val query =
    Sort("examDate", false,
      RemoveDuplicates(
        PickColumns(Seq("examDate", "examSubject"),
          ReadTable("exams"))))
  db.runQuery("knownExams", query)
}


def runTopStudentsQuery(db: Database): Unit = {
  db.deleteTable("topStudents")
  val query =
    PickColumns(Seq("firstName"),
      FilterEqual("examGrade", Entry(10),
        FilterEqual("examSubject", Entry("EIS"),
          NaturalJoin(
            ReadTable("students"),
            ReadTable("exams")))))
  db.runQuery("topStudents", query)
}


def runEarlyBirdsQuery(db: Database): Unit = {
  db.deleteTable("earlyBirds")
  val query =
    PickColumns(Seq("firstName"),
      FilterEqual("examSubject", Entry("DSEA"),
        FilterGreater("matriculationYear", Entry(2022),
          FilterSmaller("examGrade", Entry(40),
            NaturalJoin(
              ReadTable("students"),
              ReadTable("exams"))))))
  db.runQuery("earlyBirds", query)
}


def runPersonalTranscriptQuery(db: Database, name: String): Unit = {
  db.deleteTable("personalTranscript")
  val query =
    PickColumns(Seq("examDate", "examSubject", "examGrade"),
      FilterEqual("firstName", Entry(name),
        NaturalJoin(
          ReadTable("students"),
          ReadTable("exams"))))
  db.runQuery("personalTranscript", query)
}


def runFiveMostRecentExamsQuery(db: Database): Unit = {
  db.deleteTable("fiveMostRecentExams")
  val query =
    TakeFirst(5, Sort("examDate", true, ReadTable("personalTranscript")))
  db.runQuery("fiveMostRecentExams", query)
}


def runGradeDescriptionQuery(db: Database): Unit = {
  db.deleteTable("gradeDescription")
  val query =
    RenameColumn("title", "examGrade",
      PickColumns(Seq("examDate", "firstName", "examSubject", "title"),
        Sort("examGrade", false,
          NaturalJoin(
            ReadTable("students"),
            NaturalJoin(
              ReadTable("exams"),
              RenameColumn("grade", "examGrade",
                ReadTable("gradeTitles")))))))
  db.runQuery("gradeDescription", query)
}


@main def main(): Unit = {

  val db: Database = createNewDatabase

  makeTables(db)

  runKnownExamsQuery(db)
  runTopStudentsQuery(db)
  runEarlyBirdsQuery(db)
  runPersonalTranscriptQuery(db, "Maya")
  runFiveMostRecentExamsQuery(db)
  runGradeDescriptionQuery(db)

  println(db.asCsv("knownExams", true))
  println(db.asCsv("earlyBirds", false))
  println(db.asCsv("gradeDescription", true))
  println(db.asCsv("personalTranscript", true))
}
