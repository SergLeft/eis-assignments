package dbms.v2.indexing

import datastructures.UnbalancedSearchTree
import dbms.v2.misc.{DBType, RecordID, Variant}
import dbms.v2.store.Table


// use this Ordering for Variant
private val variantOrdering: Ordering[Variant] = Ordering.fromLessThan((l: Variant, r: Variant) => l < r)

class UnbalancedTreeIndex(table: Table, attribute: String)
