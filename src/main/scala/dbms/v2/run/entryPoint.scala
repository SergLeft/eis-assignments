package dbms.v2.run

import dbms.v2.misc.{Variant, IndexType, DBType}
import dbms.v2.store.{Schema, Table}

/** Program execution starts by calling this method */
@main def entryPoint(): Unit = {
  // create a schema for the students table
  val studentsSchema = Schema(Seq("studentID" -> DBType.Long, "course" -> DBType.String, "grade" -> DBType.Double, "bonus" -> DBType.Double))

  // create students table
  val students = Table(studentsSchema)
  students.appendRecord(Seq("studentID" -> Variant(42657), "course" -> Variant("CS"), "grade" -> Variant(1.3), "bonus" -> Variant(0.0)))
  students.appendRecord(Seq("studentID" -> Variant(78524), "course" -> Variant("CS"), "grade" -> Variant(1.0), "bonus" -> Variant(0.0)))
  students.appendRecord(Seq("studentID" -> Variant(66666), "course" -> Variant("CS"), "grade" -> Variant(3.7), "bonus" -> Variant(0.3)))
  students.appendRecord(Seq("studentID" -> Variant(42342), "course" -> Variant("Math"), "grade" -> Variant(2.7), "bonus" -> Variant(0.0)))
  students.appendRecord(Seq("studentID" -> Variant(97891), "course" -> Variant("Math"), "grade" -> Variant(1.7), "bonus" -> Variant(0.0)))
  students.appendRecord(Seq("studentID" -> Variant(25466), "course" -> Variant("CS"), "grade" -> Variant(1.7), "bonus" -> Variant(0.7)))
  students.appendRecord(Seq("studentID" -> Variant(89134), "course" -> Variant("Physics"), "grade" -> Variant(2.0), "bonus" -> Variant(0.0)))
  students.appendRecord(Seq("studentID" -> Variant(12345), "course" -> Variant("CS"), "grade" -> Variant(1.0), "bonus" -> Variant(0.0)))
  students.appendRecord(Seq("studentID" -> Variant(54534), "course" -> Variant("Physics"), "grade" -> Variant(2.3), "bonus" -> Variant(0.3)))
  students.appendRecord(Seq("studentID" -> Variant(78678), "course" -> Variant("CS"), "grade" -> Variant(5.0), "bonus" -> Variant(0.0)))

  // create indexes
  students.createIndex("grade", IndexType.HashIndex)
  students.createIndex("bonus", IndexType.TreeIndex)
  students.createIndex("course", IndexType.HashIndex)

  // you can put testing code here

}
