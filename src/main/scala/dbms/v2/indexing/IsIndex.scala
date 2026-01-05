package dbms.v2.indexing

import dbms.v2.misc.{IndexType, DBType, Variant, RecordID}

/** Represents the interface that any index must implement */
trait IsIndex {
  
  /** The data type that is stored in the index */
  def dataType: DBType

  /** Returns the number of keys currently indexed. */
  def numEntries: Int

  /** Adds a key and a recordID to the index.
   *
   *  Can handle keys that are already present in the index.
   *  @param key the key to index.
   *  @param recordID the recordID of the record from which the key originates
   *  @return true iff the key was already in the index
   */
  def add(key: Variant, recordID: RecordID): Unit

  /** Clears the index from all elements */
  def clear(): Unit

  /** Returns all recordIDs associated with the given key.
   *
   *  @param key the key to lookup in the index
   *  @return a sequence of all recordIDs associated with the given key (can be empty if key is not indexed)
   */
  def get(key: Variant): Seq[RecordID]
}
