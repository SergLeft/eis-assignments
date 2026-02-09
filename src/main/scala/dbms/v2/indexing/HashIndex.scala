package dbms.v2.indexing

import dbms.v2.misc.{RecordID, Variant}
import dbms.v2.store.Table
import scala.collection.mutable

/** Represents an index that is internally materialized as hash map
 * 
 *  @param table the table on which the index is built
 *  @param attribute the name of the attribute to build the index on
 */ 
class HashIndex(table: Table, attribute: String) extends MapBasedIndex(table, attribute) {

  /** The internal data structure (a HashMap) used to represent our index data. */
  protected val index: mutable.HashMap[Variant, Seq[RecordID]] = getIndexMapping.to(mutable.HashMap)

  /** Returns a string representation of this index. */
  override def toString: String = index
    .toSeq
    .sorted
    .map((value, idString) => s"value $value occurs in row(s) $idString\n").mkString("")
}
