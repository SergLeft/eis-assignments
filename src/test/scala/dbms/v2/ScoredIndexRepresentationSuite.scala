package dbms.v2

import dbms.v2.indexing.{HashIndex, TreeIndex}
import org.scalatest.funsuite.AnyFunSuite

class ScoredIndexRepresentationSuite extends AnyFunSuite, ExampleTables {

  private val expectedExamSubjectIndex = "value StringType(DB1) occurs in row(s) 12\nvalue StringType(DSEA) occurs in row(s) 5 and 6 and 7 and 8\nvalue StringType(EIP) occurs in row(s) 10 and 11\nvalue StringType(EIS) occurs in row(s) 0 and 1 and 2 and 3 and 19 and 20\nvalue StringType(KT) occurs in row(s) 9 and 16 and 17 and 18\nvalue StringType(ML) occurs in row(s) 13 and 14 and 15\nvalue StringType(PS) occurs in row(s) 4\n"
  private val expectedExamDateIndex = "value StringType(2023-04-17) occurs in row(s) 0 and 1 and 2 and 3 and 4\nvalue StringType(2023-06-06) occurs in row(s) 5 and 6 and 7 and 8\nvalue StringType(2023-06-09) occurs in row(s) 9\nvalue StringType(2023-06-12) occurs in row(s) 10 and 11\nvalue StringType(2023-06-13) occurs in row(s) 12\nvalue StringType(2023-08-08) occurs in row(s) 13 and 14 and 15\nvalue StringType(2023-09-23) occurs in row(s) 16\nvalue StringType(2024-01-14) occurs in row(s) 17 and 18\nvalue StringType(2024-02-13) occurs in row(s) 19 and 20\n"

  test("indexing examSubject with TreeIndex should yield a correct string representation") {
    val index = TreeIndex(exams, "examSubject")
    assert(index.toString == expectedExamSubjectIndex)
  }

  test("indexing examSubject with HashIndex should yield a correct string representation") {
    val index = HashIndex(exams, "examSubject")
    assert(index.toString == expectedExamSubjectIndex)
  }

  test("indexing examDate with TreeIndex should yield a correct string representation") {
    val index = TreeIndex(exams, "examDate")
    assert(index.toString == expectedExamDateIndex)
  }

  test("indexing examDate with HashIndex should yield a correct string representation") {
    val index = HashIndex(exams, "examDate")
    assert(index.toString == expectedExamDateIndex)
  }
}
