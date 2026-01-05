package dbms.v2.indexing

import dbms.v2.misc.{Variant, RecordID}

/** Represents the interface that any index, which supports range queries, must provide */
trait IsRangeIndex extends IsIndex {
  /** Returns all recordIDs that fall within the given key range
   *
   *  @param inclusiveLowerKey the inclusive lower key boundary
   *  @param exclusiveUpperKey the exclusive upper key boundary
   *  @return a sequence of all recordIDs associated with the given key (can be empty if key is not indexed)
   */
  def getRange(inclusiveLowerKey: Variant, exclusiveUpperKey: Variant): Seq[RecordID]
}
