package dbms.v2.indexing

import datastructures.UnbalancedSearchTree
import dbms.v2.misc.{DBType, RecordID, Variant}
import dbms.v2.store.Table


// use this Ordering for Variant
private val variantOrdering: Ordering[Variant] = Ordering.fromLessThan((l: Variant, r: Variant) => l < r)


/** Database index based on [[UnbalancedSearchTree]]
 * 
 * @param table the table on which the index is built
 * @param attribute the name of the attribute to build the index on
 */
class UnbalancedTreeIndex(table: Table, attribute: String) extends IsIndex {
  
  /** The internal search tree mapping each value to the respectiv list of recordIDs */
  private val index: UnbalancedSearchTree[Variant, Seq[RecordID]] = {
    val tree = UnbalancedSearchTree[Variant, Seq[RecordID]](variantOrdering)
    (0 until table.numRecords).foreach { recordID =>
      val key =table.getRecord(recordID).getValue(attribute)
      val currentIDs = tree.get(key).getOrElse(Seq())
      tree.addOrUpdate(key, currentIDs :+ recordID)
    }
    tree
  }

  /** The data type stored in the index */
  override def dataType: DBType = table.schema.getDataType(attribute)
  
  /** Returns the number of distinct keys in the index */
  override def numEntries: Int = index.size
  
  /** Adds a key and recordID to index
   * @param key the key to index
   * @param recordID the recordID of the record from which the key originates
   */
  override def add(key: Variant, recordID: RecordID): Unit = {
    val currentIDs = index.get(key).getOrElse(Seq())
    index.addOrUpdate(key, currentIDs :+ recordID)
  }
  
  /** Clears all entries from index */
  override def clear(): Unit = index.clear()
  
  /** Returns all recordIDs associated with given key
   * 
   * @param key the key to look up
   * @return a sequence of recordIDs (empty if key not indexed)
   */
  override def get(key: Variant): Seq[RecordID] = {
    if (this.dataType != key.dataType) {
      throw IllegalArgumentException("The data type of the passed key differs from that of the index")
    }
    index.get(key).getOrElse(Seq())
    }
}
