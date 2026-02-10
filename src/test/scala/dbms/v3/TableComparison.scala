package dbms.v3

import dbms.v3.interface.*

trait TableComparison {

  protected def isRowCorrect(
      schema: Seq[(String, DataType)],
      row: IndexedSeq[Entry],
      expected: Seq[Entry]
  ): Boolean = {
    if (schema.length != row.length)
      return false
    schema.map((name, dtype) => dtype).lazyZip(row).lazyZip(expected).forall((dtype, attribute, expected) => {
      dtype match {
        case DataType.NumericType => extractNumber(attribute) == extractNumber(expected)
        case DataType.TextType => extractText(attribute) == extractText(expected)
      }
    })
  }

  protected def isTableCorrect(
      db: Database,
      name: String,
      expectedSchema: Seq[(String, DataType)],
      expectedEntries: IndexedSeq[Seq[Entry]]
  ): Boolean = {
    val columns = db.getColumnNames(name)
    val namesCorrect = expectedSchema
      .map((name, dtype) => name)
      .zip(columns)
      .forall((expected, actual) => expected == actual)
    if (!namesCorrect) return false

    val dataTypes = db.getDataTypes(name)
    val typesCorrect = expectedSchema
      .map((name, dtype) => dtype)
      .zip(dataTypes)
      .forall((expected, actual) => expected == actual)
    if (!typesCorrect) return false

    val rowAmount = db.getNumberOfRows(name)
    if (rowAmount != expectedEntries.size) return false

    (0 until rowAmount).forall(i => isRowCorrect(expectedSchema, db.getRow(name, i), expectedEntries(i)))
  }
}
