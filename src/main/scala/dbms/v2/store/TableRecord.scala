package dbms.v2.store

import dbms.v2.misc.Variant

/** Represents an individual immutable record
 * 
 *  @param elems a sequence of (attribute, value) tuples
 */ 
class TableRecord(elems: Iterable[(String, Variant)]) extends Iterable[(String, Variant)] {
  if (elems.isEmpty)
    throw IllegalArgumentException("cannot create a record without attributes")

  val schema: Schema = Schema(elems.map((attribute: String, value: Variant) => attribute -> value.dataType))

  /** Maps each attribute of the record to its value. */
  protected val attributes: Map[String, Variant] = elems.toMap

  /** Returns the number of attributes of this record. */
  def numAttributes: Int = elems.size

  /** Returns true iff the record contains the given attribute. */
  def hasAttribute(attribute: String): Boolean = attributes.contains(attribute)

  /** Returns the value of a specific attribute.
   *
   *  @param attribute the name of the attribute to get the value for
   *  @return the value which corresponds to this attribute.
   *  @throws IllegalArgumentException if the passed attribute is unknown.
   */
  def getValue(attribute: String): Variant = {
    attributes.getOrElse(attribute, throw IllegalArgumentException("attribute name is unknown"))
  }

  /** Returns the value of a specific attribute.
   *
   *  @param attribute the name of the attribute to get the value for
   *  @return the value which corresponds to this attribute.
   *  @throws IllegalArgumentException if the passed attribute is unknown.
   */
  def apply(attribute: String): Variant = getValue(attribute)

  override def iterator: Iterator[(String, Variant)] = attributes.iterator

  /** Returns the textual representation of the record. */
  override def toString: String = attributes.mkString("(", ", ", ")")

  /** Returns whether an object equals this record. */
  override def equals(that: Any): Boolean = {
    that match {
      case record: TableRecord => record.attributes == this.attributes
      case _ => false
    }
  }

  override def hashCode: Int = attributes.hashCode
}
