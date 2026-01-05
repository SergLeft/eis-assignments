package dbms.v2.misc


/** Represents the index / row number of a record in a table. */
type RecordID = Int

/** Represents some available types of indexes. */
enum IndexType {
  case HashIndex, TreeIndex
}

/** Represents all supported data types of the database */
enum DBType {
  case Long, Double, String
}

object Variant {
  /** Returns a new IntType for the given Int */
  def apply(i: Long): Variant = LongType(i)
  /** Returns a new DoubleType for the given Double */
  def apply(d: Double): Variant = DoubleType(d)
  /** Returns a new StringType for the given String */
  def apply(s: String): Variant = StringType(s)
}

/** Represents all data type that can be used in the schema/table */
abstract class Variant(val dataType: DBType) extends Ordered[Variant] {
  /** Compares two variants
   * 
   *  @param that the variant to compare with
   *  @return 1 if this is larger than that, -1 if that is larger than this, and 0 otherwise
   */
  override def compare(that: Variant): Int = {
    (this, that) match {
      case (LongType(l), LongType(r)) => if l > r then 1 else if l < r then -1 else 0
      case (DoubleType(l), DoubleType(r)) => if l > r then 1 else if l < r then -1 else 0
      case (StringType(l), StringType(r)) => if l > r then 1 else if l < r then -1 else 0
      case _ => throw IllegalArgumentException("Comparison with incompatible type.")
    }
  }
}

/** Represents an Int type */
case class LongType(i: Long) extends Variant(DBType.Long)

/** Represents a Double type */
case class DoubleType(d: Double) extends Variant(DBType.Double)

/** Represents a String type */
case class StringType(d: String) extends Variant(DBType.String)
