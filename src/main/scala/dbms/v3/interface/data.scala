package dbms.v3.interface

/** Represents a data type. */
enum DataType {
  case NumericType
  case TextType
}

object Entry {
  /** Returns a new [[Entry]] object containing text. */
  def apply(s: String): Entry = TextData(s)
  /** Returns a new [[Entry]] object containing a number. */
  def apply(n: Long): Entry = NumericData(n)
}

/** Represents a single table entry, either textual or numeric. */
sealed trait Entry
case class TextData(s: String) extends Entry
case class NumericData(n: Long) extends Entry


/** Returns the data type of the specified table entry. */
def dataTypeOf(x: Entry): DataType = x match {
  case _: TextData => DataType.TextType
  case _: NumericData => DataType.NumericType
}

/** Returns the number stored in this table entry, if it exists. */
def extractNumber(x: Entry): Option[Long] = x match {
  case NumericData(number) => Some(number)
  case _ => None
}

/** Returns the text stored in this table entry, if it exists. */
def extractText(x: Entry): Option[String] = x match {
  case TextData(text) => Some(text)
  case _ => None
}
