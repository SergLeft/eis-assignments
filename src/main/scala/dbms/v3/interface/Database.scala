package dbms.v3.interface

trait Database {

  /** Returns the names of all available tables. */
  def listTables: Iterable[String]

  /** Creates a new table with the specified name and schema.
   *
   *  @param tableName the name for the new table
   *  @param columns the name and data type for each column of the new table
   *
   *  @throws TableAlreadyExistsException if a table with the specified name already exists in the database
   *  @throws IllegalSchemaException if the list of columns contains a name twice, or the list of columns is empty
   */
  def createTable(tableName: String, columns: Seq[(String, DataType)]): Unit

  /** Deletes the table with the specified name if it exists.
   *
   *  @param tableName the name of the table to delete
   *  @return true if a table was deleted as a consequence of this operation, false otherwise
   */
  def deleteTable(tableName: String): Boolean

  /** Returns a CSV representation of the specified table.
   *
   *  Textual data will be surrounded by double-quotes, numbers will be kept as-is.
   *  Double-quotes in textual data will be replaced by two double-quotes:
   *  The text `My "dogs"` will we represented as `"My ""dogs"""` in the CSV output.
   *
   *  @param tableName the name of the table to convert
   *  @param withHeader if true, generate an additional line at the beginning containing the column names as text data
   *  @throws NoSuchTableException if no table with the specified name exists in the database
   */
  def asCsv(tableName: String, withHeader: Boolean): String

  /** Adds a new row containing the specified entries to the end of the table.
   *
   *  @param tableName the name of the table to append to
   *  @param entries the entries of the new row, in the same order as the table schema
   *
   *  @throws NoSuchTableException if no table with the specified name exists in the database
   *  @throws DataTypeMismatchException if the schema of the entries does not match the specified table
   */
  def addRow(tableName: String, entries: Seq[Entry]): Unit

  /** Creates a new table with the specified name by evaluating a database query against existing tables.
   *  See [[QueryPlan]] for an overview of all possible exceptions.
   *
   *  @param resultTableName the name of the resulting table
   *  @param queryPlan the query to evaluate
   *
   *  @throws TableAlreadyExistsException if a table with the specified name already exists in the database
   */
  def runQuery(resultTableName: String, queryPlan: QueryPlan): Unit

  /** Returns the column names of the specified table.
   *
   *  @throws NoSuchTableException if no table with the specified name exists in the database
   */
  def getColumnNames(tableName: String): IndexedSeq[String]

  /** Returns the data types of the columns of the specified table.
   *
   *  @throws NoSuchTableException if no table with the specified name exists in the database
   */
  def getDataTypes(tableName: String): IndexedSeq[DataType]

  /** Returns the number of rows of the specified table.
   *
   *  @throws NoSuchTableException if no table with the specified name exists in the database
   */
  def getNumberOfRows(tableName: String): Int

  /** Returns the values in the specified row of the specified table in the same order as the column names.
   *
   *  @param tableName the name of the table to get the row from
   *  @param rowOffset the offset of the row to return
   *
   *  @throws NoSuchTableException if no table with the specified name exists in the database
   *  @throws IndexOutOfBoundsException if the row offset is negative or exceeds the number of rows
   */
  def getRow(tableName: String, rowOffset: Int): IndexedSeq[Entry]
}
