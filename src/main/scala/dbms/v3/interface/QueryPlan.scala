package dbms.v3.interface

/** Represents the steps required to produce a new table. */
sealed trait QueryPlan

/** Reads an existing table in the system, or throws [[NoSuchTableException]]. */
case class ReadTable(name: String) extends QueryPlan

/** Renames a column in the table returned by the specified sub-plan.
  * Throws [[NoSuchColumnException]] if the referenced column does not exist.
  * Throws [[ColumnAlreadyExistsException]] if a column with the new name already exists in the table. */
case class RenameColumn(oldName: String, newName: String, plan: QueryPlan) extends QueryPlan

/** Selects a subset of the columns in the table returned by the specified sub-plan.
  * Throws [[NoSuchSchemaException]] if the subset is empty.
  * Throws [[NoSuchColumnException]] the subset references an unknown column. */
case class PickColumns(columns: Seq[String], plan: QueryPlan) extends QueryPlan

/** Removes all duplicate rows in the table returned by the specified sub-plan. */
case class RemoveDuplicates(plan: QueryPlan) extends QueryPlan

/** Only retains the rows in the table returned by the specified sub-plan
  * where the value in the specified column matches the reference value.
  * Can throw [[NoSuchColumnException]] or [[DataTypeMismatchException]] if the input is faulty. */
case class FilterEqual(column: String, reference: Entry, plan: QueryPlan) extends QueryPlan

/** Only retains the rows in the table returned by the specified sub-plan
  * where the value in the specified column is smaller or equal when compared to the reference value.
  * Can throw [[NoSuchColumnException]] or [[DataTypeMismatchException]] if the input is faulty. */
case class FilterSmaller(column: String, inclUpperBound: Entry, plan: QueryPlan) extends QueryPlan

/** Only retains the rows in the table returned by the specified sub-plan
  * where the value in the specified column is greater or equal when compared to the reference value.
  * Can throw [[NoSuchColumnException]] or [[DataTypeMismatchException]] if the input is faulty. */
case class FilterGreater(column: String, inclLowerBound: Entry, plan: QueryPlan) extends QueryPlan

/** Sorts the table returned by the specified sub-plan by the specified column, or throws [[NoSuchColumnException]].
  * The sorting algorithm is stable, i.e., if two rows compare equal, their relative order remains unchanged. */
case class Sort(byColumn: String, descending: Boolean, plan: QueryPlan) extends QueryPlan

/** Joins the two tables returned by the specified sub-plans.
  * The schema of the resulting table should consist of all columns in the left table
  * followed by all columns in the right table (but without the join column).
  * Throws [[IllegalSchemaException]] if the schemas do not share exactly one column.
  * Throws [[DataTypeMismatchException]] if the shared column is not of the same data type. */
case class NaturalJoin(leftPlan: QueryPlan, rightPlan: QueryPlan) extends QueryPlan

/** Only retains the first few rows in the table returned by the specified sub-plan.
  * Throws [[IllegalArgumentException]] if the specified amount is negative. */
case class TakeFirst(amount: Int, plan: QueryPlan) extends QueryPlan

