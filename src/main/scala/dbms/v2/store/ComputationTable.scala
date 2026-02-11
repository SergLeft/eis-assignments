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
  def compute(computation: Expression): Seq[Double] = {

    // Find all variables the expression references
    val vars = referencedVariables(computation)

    // Validate; all referenced variables must exist as columns
    vars.foreach { v =>
      if (!schema.contains(v))
        throw new IllegalArgumentException(s"The computation references an unknown column: $v")
    }

    // Validate; all referenced columns must be numeric
    vars.foreach { v=>
      val dt = schema.getDataType(v)
      if (dt != DBType.Long && dt != DBType.Double)
        throw new IllegalArgumentException(s"The computation references a non-numeric column: $v")
    }
    
    // Evaluate for each row
    records.map { record =>
      // Build variable bindings: column name -> Double value
      val variables: Map[String, Double] = vars.map { v=>
        val value = record.getValue(v) match {
          case LongType(i) => i.toDouble
          case DoubleType(d) => d
          case _ => throw IllegalArgumentException("non-numeric value encountered")
        }
        v -> value
      }.toMap
      
      // Evaluate, catching ArithmeticException
      try {
        evaluate(computation, variables)
      } catch {
        case _ : ArithmeticException => 0.0
      }
    }.toSeq  
  }
}
