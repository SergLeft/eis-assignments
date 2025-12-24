package dbms.v1

import scala.collection.mutable


/** The type of each value. Currently, we only support one value type. */
type Value = Double

/** Represents the index/row-number of a record in a table. */
type RecordID = Int

/** Represents all available types of indexes. */
enum IndexType {
  case HashIndex, TreeIndex
}

/** Program execution starts by calling this method */
@main def entryPoint(): Unit = {
  // create students table
  val students = Table(Seq("studentID", "grade", "bonus"))

  students.appendRecord(Seq("studentID" -> 42657.0, "grade" -> 1.3, "bonus" -> 0.0))
  students.appendRecord(Seq("studentID" -> 78524.0, "grade" -> 1.0, "bonus" -> 0.0))
  students.appendRecord(Seq("studentID" -> 66666.0, "grade" -> 3.7, "bonus" -> 0.3))
  students.appendRecord(Seq("studentID" -> 42342.0, "grade" -> 2.7, "bonus" -> 0.0))
  students.appendRecord(Seq("studentID" -> 97891.0, "grade" -> 1.7, "bonus" -> 0.0))
  students.appendRecord(Seq("studentID" -> 25466.0, "grade" -> 1.7, "bonus" -> 0.7))
  students.appendRecord(Seq("studentID" -> 89134.0, "grade" -> 2.0, "bonus" -> 0.0))
  students.appendRecord(Seq("studentID" -> 12345.0, "grade" -> 1.0, "bonus" -> 0.0))
  students.appendRecord(Seq("studentID" -> 54534.0, "grade" -> 2.3, "bonus" -> 0.3))
  students.appendRecord(Seq("studentID" -> 78678.0, "grade" -> 5.0, "bonus" -> 0.0))

  // create indexes
  students.createIndex("grade", IndexType.HashIndex)
  students.createIndex("bonus", IndexType.TreeIndex)

  // use point index to get all studentIDs with a 1.0
  println("StudentIDs with a grade equal to 1.0:")
  val bestStudents: Table = students.filterByIndex("studentID", "grade", 1.0)
  println(bestStudents)

  println("")

  // use range index to get all studentIDs with a bonus >= 0.3 and < 1.0
  println("StudentIDs with a bonus >= 0.3 and < 1.0:")
  val highestBonus = students.filterRangeByIndex("studentID", "bonus", 0.3, 1.0)
  println(highestBonus)
}

/** Represents an individual immutable record.
 *
 *  @param elems a sequence of (attribute, value) tuples
 */
class TableRecord(elems: Seq[(String, Value)]) {


  /** Maps each attribute of the record to its value. */
  private val attributes: Map[String, Value] = elems.toMap

  /** Returns the number of attributes of this record. */
  val numAttributes: Int = elems.size

  /** Returns true iff the record contains the given attribute. */
  def hasAttribute(attribute: String): Boolean = attributes.contains(attribute)

  /** Returns the value of a specific attribute.
   *
   *  @param attribute the name of the attribute to get the value for
   *  @return the value which corresponds to this attribute.
   *  @throws IllegalArgumentException if the passed attribute is unknown.
   */
  def getValue(attribute: String): Value = {
    attributes.getOrElse(attribute, throw IllegalArgumentException("Passed attribute name is unknown."))
  }

  /** Returns the textual representation of a record. */
  override def toString: String = attributes.mkString("(", ", ", ") [") + super.toString + "]"
}

/** Represents a table of the database.
 *
 *  @constructor creates an empty table
 *  @param attributes the schema of the table
 */
class Table(attributes: Seq[String]) {


  if (attributes.size != attributes.distinct.size)
    throw new IllegalArgumentException("Cannot create a table with duplicate attributes")

  /** Represents the schema of the table in form of an immutable sequence of unique attribute-names. */
  val schema: Seq[String] = attributes

  /** Holds all records of the table in an ordered fashion. */
  protected val records: mutable.ArrayBuffer[TableRecord] = mutable.ArrayBuffer()

  /** Returns the number of records that are currently present in the table. */
  def numRecords: Int = records.size

  /** Maps an attribute name to an index */
  protected val indexes: mutable.Map[String, MapBasedIndex] = collection.mutable.Map()

  /** Appends a given record to the table.
   *
   *  @param record the Record to append
   *  @return the RecordID of the record in the table.
   */
  def appendRecord(record: TableRecord): RecordID = {
    if (record.numAttributes != schema.size)
      throw IllegalArgumentException("The passed record has a different number of attributes than the schema of the table.")

    if (schema.exists((attributeInSchema: String) => !record.hasAttribute(attributeInSchema)))
      throw IllegalArgumentException("The passed record has a different schema than the table.")

    records.append(record)
    val recordID = numRecords - 1

    // update all existing indexes to reflect the newly inserted record
    indexes.foreach((attribute: String, index: MapBasedIndex) => index.add(record.getValue(attribute), recordID))

    recordID
  }

  /** Creates a record from the given (attribute, value) tuples and appends it to the table.
   *
   *  @param elems a sequence of (attribute, value) tuples
   *  @return the RecordID of the record in the table.
   */
  def appendRecord(elems: Seq[(String, Value)]): RecordID = {
    appendRecord(TableRecord(elems))
  }

  /** Returns a record for a given recordID. */
  def getRecord(recordID: RecordID): TableRecord = {
    val recordIDs: Range = records.indices
    val validRecordID: Boolean = recordIDs.contains(recordID)
    if (!validRecordID)
      throw IllegalArgumentException("The specified recordID is out of bounds.")

    records(recordID)
  }

  /** Creates an index of a specified type on a specified attribute
   *
   * @param attribute the attribute name of the attribute on which the index should be created
   * @param indexType the type of index to create
   * @return true if a new index was actually created. Returns false, if an index already exists on the attribute.
   */
  def createIndex(attribute: String, indexType: IndexType): Unit = {
    if (!schema.contains(attribute))
      throw IllegalArgumentException("This attribute does not exist in this table.")
    if (indexes.contains(attribute))
      throw IllegalArgumentException("An index already exists for the specified attribute.")

    val newIndex: MapBasedIndex = indexType match {
      case IndexType.HashIndex => HashIndex(this, attribute)
      case IndexType.TreeIndex => TreeIndex(this, attribute)
    }

    indexes.update(attribute, newIndex)
  }

  /** Returns a new [[Table]] which contains the specified subset of the attributes in this [[Table]].
   *
   * @param outputAttributes the attributes in the resulting table
   * @throws IllegalArgumentException if no attributes have been specified
   * @throws IllegalArgumentException if an output attribute does not exist in this table
   */
  def project(outputAttributes: Seq[String]): Table = ???

  /** Filters the table using the index with respect to a key.
   *
   * @param outputAttribute the attribute of interest
   * @param selectionAttribute the attribute to filter by
   * @param key the key to match with selectionAttribute, such that the record becomes part of the result
   * @return the resulting table
   * @throws IllegalArgumentException if the selection attribute does not exist
   * @throws IllegalArgumentException if the output attribute does not exist
   */
  def filterByIndex(outputAttribute: String, selectionAttribute: String, key: Value): Table = {
    if (!attributes.contains(outputAttribute))
      throw new IllegalArgumentException(s"no such attribute: $outputAttribute")
    if (!attributes.contains(selectionAttribute))
      throw new IllegalArgumentException(s"no such attribute: $selectionAttribute")
    indexes.get(selectionAttribute) match {
      case Some(index) => {
        val qualifyingRecords = index
          .get(key)
          .map(recordID => getRecord(recordID))
        val outputRecords = qualifyingRecords
          .map(record => TableRecord(Seq(outputAttribute -> record.getValue(outputAttribute))))
        val resultTable = Table(Seq(outputAttribute))
        outputRecords.foreach(record => resultTable.appendRecord(record))
        resultTable
      }
      case None => throw IllegalArgumentException("There is no index supporting point queries available for this attribute!")
    }
  }

  /** Filters the table using the index with respect to a key range.
   *
   * @param outputAttribute the attribute of interest
   * @param selectionAttribute the attribute to filter by
   * @param inclusiveLowerKey the inclusive lower key boundary for the selectionAttribute
   * @param exclusiveUpperKey the exclusive upper key boundary for the selectionAttribute
   * @return the resulting table
   * @throws IllegalArgumentException if the selection attribute does not exist
   * @throws IllegalArgumentException if the output attribute does not exist
   */
  def filterRangeByIndex(outputAttribute: String, selectionAttribute: String, inclusiveLowerKey: Value, exclusiveUpperKey: Value): Table = {
    if (!attributes.contains(outputAttribute))
      throw new IllegalArgumentException(s"no such attribute: $outputAttribute")
    if (!attributes.contains(selectionAttribute))
      throw new IllegalArgumentException(s"no such attribute: $selectionAttribute")
    indexes.get(selectionAttribute) match {
      case Some(index: TreeIndex) => {
        val qualifyingRecords = index
          .getRange(inclusiveLowerKey, exclusiveUpperKey)
          .map(recordID => getRecord(recordID))
        val outputRecords = qualifyingRecords
          .map(record => TableRecord(Seq(outputAttribute -> record.getValue(outputAttribute))))
        val resultTable = Table(Seq(outputAttribute))
        outputRecords.foreach(record => resultTable.appendRecord(record))
        resultTable
      }
      case Some(_) => throw IllegalArgumentException("There is no index supporting range queries available for this attribute!")
      case None => throw IllegalArgumentException("There is no index available for this attribute!")
    }
  }

  /** Returns a new [[Table]] containing all rows where the specified attribute has the specified value.
   *
   * @param outputAttribute    the attribute of interest
   * @param selectionAttribute the attribute to filter by
   * @param requiredValue      the value to compare against
   * @throws IllegalArgumentException if the selection attribute does not exist
   * @throws IllegalArgumentException if the output attribute does not exist
   */
  def filterByScan(outputAttribute: String, selectionAttribute: String, requiredValue: Value): Table = ???

  /** Filters the table with respect to a key using a full scan.
   *
   * @param outputAttribute    the attribute of interest
   * @param selectionAttribute the attribute to filter by
   * @param inclusiveLowerKey  the inclusive lower key boundary on the selectionAttribute
   * @param exclusiveUpperKey  the exclusive upper key boundary on the selectionAttribute
   * @return the result table
   * @throws IllegalArgumentException if the selection attribute does not exist
   * @throws IllegalArgumentException if the output attribute does not exist
   */
  def filterRangeByScan(outputAttribute: String, selectionAttribute: String, inclusiveLowerKey: Value, exclusiveUpperKey: Value): Table = ???

  /** Returns a textual representation of all records currently stored in the table. */
  override def toString: String = records.mkString("Table:\n", "\n", "")
}

/** Represents an index. */
abstract class MapBasedIndex(table: Table, attribute: String) {
  // Requires each inheriting index to use a Map as internal data structure.
  protected val index: collection.mutable.Map[Value, Seq[RecordID]]

  /** Returns a mapping that represents the index. */
  protected def buildIndexMapping: Map[Value, Seq[RecordID]] = {
    (0 until table.numRecords)
      .groupBy(recordID => table.getRecord(recordID).getValue(attribute))
  }

  /** Retrieves all recordIDs associated with the given key
   *
   *  @param key the key to lookup in the index
   *  @return a sequence of all recordIDs associated with the given key (can be empty if key is not indexed)
   */
  def get(key: Value): Seq[RecordID] = index.getOrElse(key, Seq())

  /** Adds a key and a recordID to the index.
   *
   *  Can handle keys, that are already present in the index.
   *  @param key the key to index.
   *  @param recordID the recordID of the record from which the key originates
   *  @return true iff the key was already in the index
   */
  def add(key: Value, recordID: RecordID): Unit = {
    val currentRecordIDs = index.getOrElse(key, Seq())
    val updatedRecordIDs = currentRecordIDs.appended(recordID)
    index.update(key, updatedRecordIDs)
  }
}

/** Represents an index, that is internally materialized as hash map
 *
 *  @param table the table on which the index is built
 *  @param attribute the name of the attribute to build the index on
 */
class HashIndex(table: Table, attribute: String) extends MapBasedIndex(table, attribute) {
  /** The internal data structure (a HashMap) used to represent our index data. */
  protected val index: mutable.HashMap[Value, Seq[RecordID]] = buildIndexMapping.to(mutable.HashMap)
}

/** Represents an index, that is internally materialized as a search tree
 *
 *  @param table the table on which the index is built
 *  @param attribute the name of the attribute to build the index on
 */
class TreeIndex(table: Table, attribute: String) extends MapBasedIndex(table, attribute) {
  /** The internal data structure (a TreeMap) used to represent our index data. */
  val index: mutable.TreeMap[Value, Seq[RecordID]] = buildIndexMapping.to(mutable.TreeMap)

  /** Returns all recordIDs that fall within the given key range
   *
   * @param inclusiveLowerKey the inclusive lower key boundary
   * @param exclusiveUpperKey the exclusive upper key boundary
   * @return a sequence of all recordIDs associated with the given key (can be empty if key is not indexed)
   */
  def getRange(inclusiveLowerKey: Value, exclusiveUpperKey: Value): Seq[RecordID] = {
    index.range(inclusiveLowerKey, exclusiveUpperKey)
      .flatMap((key, recordIDs) => recordIDs)
      .toSeq
  }
}


/** Returns a new table containing the IDs of the students whose grade is equal to 4.0.
 *
 *  @param students the table of all registered students
 */
def closeCall(students: Table): Table = ???

/** Returns a new table containing the IDs of the students whose grade is better than 5.0.
 *
 *  @param students the table of all registered students
 */
def passed(students: Table): Table = ???

/** Returns true if the specified student ID occurs in the table of registered students, false otherwise.
 *
 *  @param students the table of all registered students
 *  @param requestedStudent the ID to look for
 */
def doesStudentExist(students: Table, requestedStudent: Value): Boolean = ???

/** Returns the number of students who receive grade 1.0 but no bonus.
 *
 *  @param students the table of all registered students
 */
def countBestGradeNoBonus(students: Table): Int = ???


/** Represents a database table that uses statistics to answer queries more efficiently.
 *
 *  @constructor creates an empty table
 *  @param attributes the schema of the table
 */
class StatsTable(attributes: Seq[String]) extends Table(attributes) {
  private val attributeMin = mutable.Map[String, Value]()
  private val attributeMax = mutable.Map[String, Value]()

  // override methods here

  def minimum(attribute: String): Value = ???

  def maximum(attribute: String): Value = ???

}
