package dbms.v2.run

import dbms.v2.misc.Variant
import dbms.v2.store.Table


/** Returns a [[Table]] containing all known exams with their respective dates and subjects.
 *
 *  @param students a [[Table]] containing student info
 *  @param exams a [[Table]] containing a list of exam results
 */
def knownExams(students: Table, exams: Table): Table = ???

/** Returns a [[Table]] containing all students scoring the highest grade on the software design exam.
 *
 *  @param students a [[Table]] containing student info
 *  @param exams a [[Table]] containing a list of exam results
 */
def topStudents(students: Table, exams: Table): Table = ???

/** Returns a [[Table]] containing all students who were matriculated 2022 or later and also passed the DSEA exam.
 *
 *  @param students a [[Table]] containing student info
 *  @param exams a [[Table]] containing a list of exam results
 */
def earlyBirds(students: Table, exams: Table): Table = ???

/** Returns a [[Table]] containing all exam results for the specified student.
 *
 *  @param students a [[Table]] containing student info
 *  @param exams a [[Table]] containing a list of exam results
 *  @param studentName the student to create the transcript for
 */
def personalTranscript(students: Table, exams: Table, studentName: String): Table = ???
