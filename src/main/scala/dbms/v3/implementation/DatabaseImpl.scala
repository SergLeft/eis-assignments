package dbms.v3.implementation

import dbms.v3.interface.*
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/** Internal representation of a table. Not exposed to the user. */
private class InternalTable(
                             val columnNames: IndexedSeq[String],
                             val columnTypes: IndexedSeq[DataType],
                             val rows: ArrayBuffer[IndexedSeq[Entry]] = ArrayBuffer()
                           )


/** A concrete implementation of the [[Database]] interface. */
class DatabaseImpl extends Database {

  /** All tables, keyed by name. */
  private val tables: mutable.HashMap[String, InternalTable] = mutable.HashMap()

  // Helper: get a table or throw

  /** Returns the table with the given name, or throws [[NoSuchTableException]]. */
  private def getTable(name: String): InternalTable =
    tables.getOrElse(name, throw NoSuchTableException(s"Table '$name' does not exist.", name))

  // Table management

  override def listTables: Iterable[String] = tables.keys

  override def createTable(tableName: String, columns: Seq[(String, DataType)]): Unit = {
    if (tables.contains(tableName))
      throw TableAlreadyExistsException(s"Table '$tableName' already exists.", tableName)
    if (columns.isEmpty)
      throw IllegalSchemaException("Column list must not be empty.")
    if (columns.map(_._1).distinct.size != columns.size)
      throw IllegalSchemaException("Column names must be unique.")

    val colNames = columns.map(_._1).toIndexedSeq
    val colTypes = columns.map(_._2).toIndexedSeq
    tables(tableName) = InternalTable(colNames, colTypes)
  }

  override def deleteTable(tableName: String): Boolean = tables.remove(tableName).isDefined

  // Row access

  override def addRow(tableName: String, entries: Seq[Entry]): Unit = {
    val table = getTable(tableName)
    if (entries.size != table.columnNames.size)
      throw DataTypeMismatchException("Number of entries does not match number of columns.")
    entries.zip(table.columnTypes).foreach { (entry, expectedType) =>
      if (dataTypeOf(entry) != expectedType)
        throw DataTypeMismatchException(
          s"Expected $expectedType but got ${dataTypeOf(entry)}.")
    }
    table.rows += entries.toIndexedSeq
  }

  override def getColumnNames(tableName: String): IndexedSeq[String] =
    getTable(tableName).columnNames

  override def getDataTypes(tableName: String): IndexedSeq[DataType] =
    getTable(tableName).columnTypes

  override def getNumberOfRows(tableName: String): Int =
    getTable(tableName).rows.size

  override def getRow(tableName: String, rowOffset: Int): IndexedSeq[Entry] = {
    val table = getTable(tableName)
    if (rowOffset < 0 || rowOffset >= table.rows.size)
      throw IndexOutOfBoundsException(s"Row offset $rowOffset is out of bounds.")
    table.rows(rowOffset)
  }

  // CSV export

  override def asCsv(tableName: String, withHeader: Boolean): String = {
    val table = getTable(tableName)
    val sb = StringBuilder()

    // Header line (column names treated as text data)
    if (withHeader) {
      sb.append(table.columnNames.map(name => csvEscapeText(name)).mkString(","))
      sb.append("\n")
    }

    // Data rows
    table.rows.foreach { row =>
      val line = row.map {
        case TextData(s) => csvEscapeText(s)
        case NumericData(n) => n.toString
      }.mkString(",")
      sb.append(line)
      sb.append("\n")
    }

    sb.toString
  }

  /** Wraps text in double quotes, escaping any internal double quotes by doubling them. */
  private def csvEscapeText(s: String): String =
    "\"" + s.replace("\"", "\"\"") + "\""

  // Query execution

  override def runQuery(resultTableName: String, queryPlan: QueryPlan): Unit = {
    if (tables.contains(resultTableName))
      throw TableAlreadyExistsException(s"Table '$resultTableName' already exists.", resultTableName)

    val result = evaluateQuery(queryPlan)
    tables(resultTableName) = result
  }

  /** Recursively evaluates a [[QueryPlan]] and returns an [[InternalTable]]. */
  private def evaluateQuery(plan: QueryPlan): InternalTable = plan match {

    // ReadTable: just look up the table
    case ReadTable(name) =>
      val src = getTable(name)
      // Return a COPY so mutations don't affect the original
      InternalTable(src.columnNames, src.columnTypes, src.rows.clone())

    // RenameColumn: change one column name
    case RenameColumn(oldName, newName, subPlan) =>
      val table = evaluateQuery(subPlan)
      val idx = columnIndex(table, oldName)
      if (table.columnNames.contains(newName))
        throw ColumnAlreadyExistsException(s"Column '$newName' already exists.", newName)
      val newNames = table.columnNames.updated(idx, newName)
      InternalTable(newNames, table.columnTypes, table.rows)

    // PickColumns: project to a subset of columns
    case PickColumns(columns, subPlan) =>
      val table = evaluateQuery(subPlan)
      if (columns.isEmpty)
        throw IllegalSchemaException("Column selection must not be empty.")
      val indices = columns.map(c => columnIndex(table, c))
      val newNames = indices.map(table.columnNames(_)).toIndexedSeq
      val newTypes = indices.map(table.columnTypes(_)).toIndexedSeq
      val newRows = table.rows.map(row => indices.map(row(_)).toIndexedSeq)
      InternalTable(newNames, newTypes, newRows)

    // RemoveDuplicates: distinct rows
    case RemoveDuplicates(subPlan) =>
      val table = evaluateQuery(subPlan)
      val distinctRows = table.rows.distinct
      InternalTable(table.columnNames, table.columnTypes, distinctRows.to(ArrayBuffer))

    // FilterEqual: keep rows where column == reference
    case FilterEqual(column, reference, subPlan) =>
      val table = evaluateQuery(subPlan)
      val idx = columnIndex(table, column)
      checkDataType(table, idx, reference)
      val filtered = table.rows.filter(row => row(idx) == reference)
      InternalTable(table.columnNames, table.columnTypes, filtered)

    // FilterSmaller: keep rows where column <= inclUpperBound
    case FilterSmaller(column, inclUpperBound, subPlan) =>
      val table = evaluateQuery(subPlan)
      val idx = columnIndex(table, column)
      checkDataType(table, idx, inclUpperBound)
      val filtered = table.rows.filter(row => compareEntries(row(idx), inclUpperBound) <= 0)
      InternalTable(table.columnNames, table.columnTypes, filtered)

    // FilterGreater: keep rows where column >= inclLowerBound
    case FilterGreater(column, inclLowerBound, subPlan) =>
      val table = evaluateQuery(subPlan)
      val idx = columnIndex(table, column)
      checkDataType(table, idx, inclLowerBound)
      val filtered = table.rows.filter(row => compareEntries(row(idx), inclLowerBound) >= 0)
      InternalTable(table.columnNames, table.columnTypes, filtered)

    // Sort: stable sort by one column
    case Sort(byColumn, descending, subPlan) =>
      val table = evaluateQuery(subPlan)
      val idx = columnIndex(table, byColumn)
      val sorted = table.rows.sortWith { (a, b) =>
        val cmp = compareEntries(a(idx), b(idx))
        if (descending) cmp > 0 else cmp < 0
      }
      InternalTable(table.columnNames, table.columnTypes, sorted)

    // NaturalJoin: join on exactly one shared column
    case NaturalJoin(leftPlan, rightPlan) =>
      val left = evaluateQuery(leftPlan)
      val right = evaluateQuery(rightPlan)

      // Find shared columns
      val shared = left.columnNames.filter(right.columnNames.contains(_))
      if (shared.size != 1)
        throw IllegalSchemaException("Natural join requires exactly one shared column.")

      val joinCol = shared.head
      val leftIdx = left.columnNames.indexOf(joinCol)
      val rightIdx = right.columnNames.indexOf(joinCol)

      if (left.columnTypes(leftIdx) != right.columnTypes(rightIdx))
        throw DataTypeMismatchException("Shared column has different data types.")

      // Build result schema: all left columns + right columns except the join column
      val rightIndicesWithoutJoin = right.columnNames.indices.filter(_ != rightIdx)
      val newNames = left.columnNames ++ rightIndicesWithoutJoin.map(right.columnNames(_))
      val newTypes = left.columnTypes ++ rightIndicesWithoutJoin.map(right.columnTypes(_))

      // Perform the join
      val joinedRows = ArrayBuffer[IndexedSeq[Entry]]()
      left.rows.foreach { leftRow =>
        val joinValue = leftRow(leftIdx)
        right.rows.foreach { rightRow =>
          if (rightRow(rightIdx) == joinValue) {
            val rightEntries = rightIndicesWithoutJoin.map(rightRow(_))
            joinedRows += (leftRow ++ rightEntries).toIndexedSeq
          }
        }
      }
      InternalTable(newNames.toIndexedSeq, newTypes.toIndexedSeq, joinedRows)

    // TakeFirst: keep only the first N rows
    case TakeFirst(amount, subPlan) =>
      if (amount < 0)
        throw IllegalArgumentException(s"Amount must not be negative: $amount")
      val table = evaluateQuery(subPlan)
      val taken = table.rows.take(amount)
      InternalTable(table.columnNames, table.columnTypes, taken.to(ArrayBuffer))
  }

  // Query helpers

  /** Returns the index of the named column, or throws [[NoSuchColumnException]]. */
  private def columnIndex(table: InternalTable, name: String): Int = {
    val idx = table.columnNames.indexOf(name)
    if (idx < 0) throw NoSuchColumnException(s"Column '$name' does not exist.", name)
    idx
  }

  /** Checks that the reference entry's type matches the column's type. */
  private def checkDataType(table: InternalTable, colIdx: Int, reference: Entry): Unit = {
    if (dataTypeOf(reference) != table.columnTypes(colIdx))
      throw DataTypeMismatchException(
        s"Expected ${table.columnTypes(colIdx)} but got ${dataTypeOf(reference)}.")
  }

  /** Compares two entries of the same type. Text is compared lexicographically, numbers numerically. */
  private def compareEntries(a: Entry, b: Entry): Int = (a, b) match {
    case (NumericData(n1), NumericData(n2)) => n1.compareTo(n2)
    case (TextData(s1), TextData(s2)) => s1.compareTo(s2)
    case _ => throw DataTypeMismatchException("Cannot compare entries of different types.")
  }
}
