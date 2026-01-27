package dbms.v2.store

import dbms.v2.misc.*
import expressions.{Expression, referencedVariables, evaluate}


/** A [[Table]] with the ability to evaluate expressions.
 *
 *  @param schema the schema of the table
 */
class ComputationTable(schema: Schema) extends Table(schema) {

  def this(schema: Schema, initialRecords: Iterable[TableRecord]) = {
    this(schema)
    initialRecords.foreach((r: TableRecord) => appendRecord(r))
  }

  /** Returns a sequence containing the result of evaluating the specified computation for each row of this [[Table]].
   *
   * @param computation the computation to run for each row
   * @throws IllegalArgumentException if the computation references an unknown column
   * @throws IllegalArgumentException if the computation references a non-numeric column
   */
  def compute(computation: Expression): Seq[Double] = ???

}
