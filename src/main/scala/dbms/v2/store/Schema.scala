package dbms.v2.store

import dbms.v2.misc.DBType

/** Represents an immutable schema 
 * 
 *  @param elems a sequence of (attribute name, attribute type) pairs
 */
class Schema(elems: Iterable[(String, DBType)]) extends Iterable[(String, DBType)] {
  if (elems.isEmpty) 
    throw IllegalArgumentException("cannot create a schema without attributes")

  if (elems.size != elems.map((attr, dataType) => attr).toSeq.distinct.size)
    throw IllegalArgumentException("attributes are not unique")

  /** Maps each attribute to its data type*/
  protected val dataTypes: Map[String, DBType] = elems.toMap

  /** All attribute names of this schema */
  def attributes: Set[String] = dataTypes.keySet

  /** The number of attributes of the schema */
  def numAttributes: Int = attributes.size

  /** Returns whether an attribute with the specified attributeName is part of the schema */
  def contains(attribute: String): Boolean = attributes.contains(attribute)

  /** Returns the DBType associated with the passed attribute */
  def getDataType(attribute: String): DBType = dataTypes
    .getOrElse(attribute, throw IllegalArgumentException("attribute is not part of the schema"))

  /** Returns a new schema containing only the passed subsetOfAttributes */
  def getSubsetOfAttributes(subsetOfAttributes: Seq[String]): Schema = {
    if (subsetOfAttributes.exists((attribute: String) => !contains(attribute)))
      throw IllegalArgumentException("the provided subset of attribute is not a subset of the schema")

    val filteredAttributes = dataTypes.filter((attribute: String, dataType: DBType) => subsetOfAttributes.contains(attribute))
    Schema(filteredAttributes.toSeq)
  }

  override def iterator: Iterator[(String, DBType)] = dataTypes.iterator

  /** Returns whether that equals with this in terms of the maps
   *
   *  @param that the Schema to compare with
   *  @return true, iff this equals that 
   */ 
  override def equals(that: Any): Boolean = {
    that match {
      case thatSchema: Schema => this.dataTypes == thatSchema.dataTypes
      case _ => false
    }
  }

  /** Delegates to the hashCode of the attributes */ 
  override def hashCode: Int = dataTypes.hashCode
}
