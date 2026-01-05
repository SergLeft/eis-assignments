package dbms.v2.indexing

import dbms.v2.misc.{DBType, Variant, RecordID, IndexType}
import dbms.v2.store.Table

/** Represents an index. */ 
abstract class MapBasedIndex(table: Table, attribute: String) extends IsIndex {
  
  /** Requires each inheriting index to use a Map as internal data structure. */
  protected val index: collection.mutable.Map[Variant, Seq[RecordID]]

  /** The data type that is stored in the index */
  override def dataType: DBType = table.schema.getDataType(attribute)

  /** Returns a mapping that represents the index. */
  protected def getIndexMapping: Map[Variant, Seq[RecordID]] = {
    (0 until table.numRecords)
      .groupBy(recordID => table.getRecord(recordID).getValue(attribute))
  }

  /** Returns the number of keys currently indexed. */
  override def numEntries: Int = index.size

  /** Adds a key and a recordID to the index.
   *
   *  Can handle keys that are already present in the index.
   *  @param key the key to index.
   *  @param recordID the recordID of the record from which the key originates
   *  @return true iff the key was already in the index
   */
  def add(key: Variant, recordID: RecordID): Unit = {
    val currentRecordIDs = index.getOrElse(key, Seq())
    val updatedRecordIDs = currentRecordIDs.appended(recordID)
    index.update(key, updatedRecordIDs)
  }  

  /** Clears the index from all elements */
  override def clear(): Unit = index.clear

  /** Retrieves all recordIDs associated with the given key 
   * 
   *  @param key the key to lookup in the index
   *  @return a sequence of all recordIDs associated with the given key (can be empty if key is not indexed)
   */
  def get(key: Variant): Seq[RecordID] = {
    if (this.dataType != key.dataType)
      throw IllegalArgumentException("The datatype of the passed key differs from the datatype of the index.")
          
    index.getOrElse(key, Seq())   
  }
}