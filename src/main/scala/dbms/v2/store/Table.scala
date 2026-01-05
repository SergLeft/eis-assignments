package dbms.v2.store

import dbms.v2.misc.{IndexType, RecordID, Variant}
import dbms.v2.indexing.{HashIndex, IsIndex, IsRangeIndex, TreeIndex}
import collection.mutable.ArrayBuffer


/** Represents a table of the database
 * 
 *  @constructor creates an empty table
 *  @param schema the schema of the table
 */
class Table(val schema: Schema) extends Iterable[TableRecord] {

  /** Alternative constructor to bulk-load the created table 
   *  
   *  @param schema the schema of the table
   *  @param initialRecords a sequence of records to append to the table
   */
  def this(schema: Schema, initialRecords: Iterable[TableRecord]) = {
    this(schema)
    initialRecords.foreach((r: TableRecord) => appendRecord(r))
  }

  /** Holds all records of the table in an ordered fashion. */
  protected val records: ArrayBuffer[TableRecord] = ArrayBuffer()

  /** Returns the number of records that are currently present in the table. */
  def numRecords: Int = records.size 

  /** Maps an attribute name to an index */
  protected val indexes: collection.mutable.Map[String, IsIndex] = collection.mutable.Map()

  /** Returns an iterator over all records of the table (from Iterable) */
  override def iterator: Iterator[TableRecord] = TableIterator(this) // we could also delegate to records.iterator here

  /** Appends a given record to the table.
   * 
   *  @param record the Record to append
   *  @return the RecordID of the record in the table.
   */
  def appendRecord(record: TableRecord): RecordID = {
    if (this.schema != record.schema) 
      throw IllegalArgumentException("record has a different schema than the table")

    records.append(record)
    val recordID = numRecords - 1

    // update all existing indexes to reflect the newly inserted record
    indexes.foreach((attribute: String, index: IsIndex) => index.add(record.getValue(attribute), recordID))

    recordID
  }

  /** Creates a record from the given (attribute, value) tuples and appends it to the table. 
   * 
   *  @param elems a sequence of (attribute, value) tuples
   *  @return the RecordID of the record in the table.
   */
  def appendRecord(elems: Seq[(String, Variant)]): RecordID = appendRecord(TableRecord(elems))

  /** Returns a record for a given recordID. */
  def getRecord(recordID: RecordID): TableRecord = {
    val recordIDs: Range = records.indices
    val validRecordID: Boolean = recordIDs.contains(recordID)
    if (!validRecordID)
      throw IllegalArgumentException("The passed recordID is out of bounds.")

    records(recordID)
  }

  /** Creates an index of a specified type for a specified attribute
   *
   *  @param attribute the attribute name of the attribute on which the index should be created
   *  @param indexType the type of index to create
   */
  def createIndex(attribute: String, indexType: IndexType): Unit = {
    indexType match {
      case IndexType.HashIndex => createIndex(attribute, (t, attr) => HashIndex(t, attr))
      case IndexType.TreeIndex => createIndex(attribute, (t, attr) => TreeIndex(t, attr))
    }
  }

  /** Creates an index for a specified attribute using the supplied index factory.
   *
   * @param attribute the attribute name of the attribute on which the index should be created
   * @param factory a method creating a new index from a table and an attribute name
   */
  def createIndex(attribute: String, factory: (Table, String) => IsIndex): Unit = {
    if (!schema.contains(attribute))
      throw IllegalArgumentException("This attribute does not exist in this table.")
    if (indexes.contains(attribute))
      throw IllegalArgumentException("An index already exists for the specified attribute.")

    val newIndex: IsIndex = factory(this, attribute)
    indexes.update(attribute, newIndex)
  }

  /** Filters the table using the index with respect to a key.
   *
   * @param selectionAttribute the attribute to filter by
   * @param key                the key to match with selectionAttribute, such that the record becomes part of the result
   * @return the resulting table
   * @throws IllegalArgumentException if the selection attribute does not exist
   * @throws IllegalArgumentException if the output attribute does not exist
   */
  def filterByIndex(selectionAttribute: String, key: Variant): Table = {
    indexes.get(selectionAttribute) match {
      case Some(index) =>
        val qualifyingRecords = index
          .get(key)
          .map(recordID => getRecord(recordID))
        Table(schema, qualifyingRecords)
      case None => throw IllegalArgumentException("There is no index supporting point queries available for this attribute!")
    }
  }

  /** Filters the table using the index with respect to a key range.
   *
   * @param selectionAttribute the attribute to filter by
   * @param inclusiveLowerKey  the inclusive lower key boundary for the selectionAttribute
   * @param exclusiveUpperKey  the exclusive upper key boundary for the selectionAttribute
   * @return the resulting table
   * @throws IllegalArgumentException if the selection attribute does not exist
   * @throws IllegalArgumentException if the output attribute does not exist
   */
  def filterRangeByIndex(selectionAttribute: String, inclusiveLowerKey: Variant, exclusiveUpperKey: Variant): Table = {
    indexes.get(selectionAttribute) match {
      case Some(index: IsRangeIndex) =>
        val qualifyingRecords = index
          .getRange(inclusiveLowerKey, exclusiveUpperKey)
          .map(recordID => getRecord(recordID))
        Table(schema, qualifyingRecords)
      case Some(_) => throw IllegalArgumentException("There is no index supporting range queries available for this attribute!")
      case None => throw IllegalArgumentException("There is no index available for this attribute!")
    }
  }

  /** Returns a new [[Table]] containing all rows where the specified attribute has the specified value.
   *
   * @param selectionAttribute the attribute to filter by
   * @param requiredValue      the value to compare against
   * @throws IllegalArgumentException if the selection attribute does not exist
   * @throws IllegalArgumentException if the output attribute does not exist
   */
  def filterByScan(selectionAttribute: String, requiredValue: Variant): Table = {
    if (!schema.contains(selectionAttribute))
      throw new IllegalArgumentException(s"no such attribute: $selectionAttribute")

    val qualifyingRecords = records
      .filter(r => r.getValue(selectionAttribute) == requiredValue)
    Table(schema, qualifyingRecords)
  }

  /** Filters the table with respect to a key using a full scan.
   *
   * @param selectionAttribute the attribute to filter by
   * @param inclusiveLowerKey  the inclusive lower key boundary on the selectionAttribute
   * @param exclusiveUpperKey  the exclusive upper key boundary on the selectionAttribute
   * @return the result table
   * @throws IllegalArgumentException if the selection attribute does not exist
   * @throws IllegalArgumentException if the output attribute does not exist
   */
  def filterRangeByScan(selectionAttribute: String, inclusiveLowerKey: Variant, exclusiveUpperKey: Variant): Table = {
    if (!schema.contains(selectionAttribute))
      throw new IllegalArgumentException(s"no such attribute: $selectionAttribute")

    val qualifyingRecords = records
      .filter(r => r.getValue(selectionAttribute) >= inclusiveLowerKey)
      .filter(r => r.getValue(selectionAttribute) < exclusiveUpperKey)
    Table(schema, qualifyingRecords)
  }

  /** Returns a new [[Table]] which contains the specified subset of the attributes in this [[Table]].
   *
   * @param outputAttributes the attributes in the resulting table
   * @throws IllegalArgumentException if no attributes have been specified
   * @throws IllegalArgumentException if an output attribute does not exist in this table
   */
  def project(outputAttributes: Seq[String]): Table = {
    val newSchema = schema.getSubsetOfAttributes(outputAttributes)
    val newRecords = records
      .map(r => outputAttributes.map(attr => attr -> r.getValue(attr)))
      .map(values => TableRecord(values))
    Table(newSchema, newRecords)
  }

  /** Returns a new [[Table]] without duplicate records. */
  def distinct: Table = Table(schema, records.distinct)

  /** Returns a textual representation of all records currently stored in the table. */
  override def toString: String = records.mkString("Table:\n", "\n", "")

  /** Joins two tables sharing exactly one attribute. */
  def naturalJoin(other: Table): Table = ???

  /** Returns a new table that is sorted by the given attributes */
  def sortBy(attribute: String): Table = ???

  /** Returns a new table that is sorted by the given attributes */
  def sortBy(attributes: Seq[String]): Table = ???
}
