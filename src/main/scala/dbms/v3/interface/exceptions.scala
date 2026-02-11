package dbms.v3.interface

/** Base class for all exceptions related to the database. */
class DatabaseException(message: String) extends RuntimeException(message)

/** Thrown when a user attempts to create a table with a name already present in the database. */
class TableAlreadyExistsException(message: String, val requestedTable: String) extends DatabaseException(message)

/** Thrown when a user references a table that does not exist in the database. */
class NoSuchTableException(message: String, val requestedTable: String) extends DatabaseException(message)

/** Thrown when a query references a column that does not exist in the table. */
class NoSuchColumnException(message: String, val requestedColumn: String) extends DatabaseException(message)

/** Thrown when a query tries to override an already existing column. */
class ColumnAlreadyExistsException(message: String, val requestedColumn: String) extends DatabaseException(message)

/** Thrown when a supplied [[Entry]] does not conform to the expected data type. */
class DataTypeMismatchException(message: String) extends DatabaseException(message)

/** Thrown when the table schema does not conform to the specification. */
class IllegalSchemaException(message: String) extends DatabaseException(message)

/** Thrown when the system cannot handle a certain data type. */
class UnsupportedDataTypeException(message: String, val dataType: DataType) extends DatabaseException(message)
