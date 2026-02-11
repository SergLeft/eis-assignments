package dbms.v3

import dbms.v3.interface.{DataType, Database, Entry}


trait ExampleTables {

  private given Conversion[String, Entry] = (text: String) => Entry(text)
  private given Conversion[Int, Entry] = (num: Int) => Entry(num)
  private given Conversion[Long, Entry] = (num: Long) => Entry(num)

  protected val employeesTableName: String = "Employees"
  protected val organizationTableName: String = "Organization"
  protected val productsTableName: String = "Products"

  protected val employeesSchema: Seq[(String, DataType)] = Seq(
    "Name" -> DataType.TextType,
    "Salary" -> DataType.NumericType,
    "Bonus" -> DataType.NumericType,
    "Department" -> DataType.TextType
  )

  protected val renamedEmployeesSchema: Seq[(String, DataType)] = Seq(
    "Name" -> DataType.TextType,
    "Salary" -> DataType.NumericType,
    "Additional Salary" -> DataType.NumericType,
    "Department" -> DataType.TextType
  )

  protected val organizationSchema: Seq[(String, DataType)] = Seq(
    "Name" -> DataType.TextType,
    "Department" -> DataType.TextType,
    "BuildingID" -> DataType.NumericType,
    "Floor" -> DataType.NumericType
  )

  protected val productsSchema: Seq[(String, DataType)] = Seq(
    "ProductName" -> DataType.TextType,
    "EngineerName" -> DataType.TextType,
    "DevelopmentCost" -> DataType.NumericType
  )

  protected val employeesTable: IndexedSeq[Seq[Entry]] = IndexedSeq(
    Seq("Bob", 3400, 320, "Marketing"),
    Seq("Alice", 6766, 600, "Engineering"),
    Seq("Clarissa", 10000, 1000, "Management"),
    Seq("Mark", 3400, 200, "Marketing"),
    Seq("Nina", 6766, 200, "Engineering"),
    Seq("Phillip", 5400, 600, "Finance"),
    Seq("Jeff", 15000, 18014398509481994L, "CEO")
  )

  protected val organizationTable: IndexedSeq[Seq[Entry]] = IndexedSeq(
    Seq("Aldi", "Management", 3400, 5),
    Seq("Rewe", "Marketing", 6766, 2),
    Seq("Lidl", "Engineering", 2, 0),
    Seq("MVB", "Finance", 0, 3),
    Seq("Phillip", "Finance", 0, 3)
  )

  protected val productsTable: IndexedSeq[Seq[Entry]] = IndexedSeq(
    Seq("MicroMUX", "Alice", 58000),
    Seq("MicroMUX", "Alice", 58000),
    Seq("Advanced Graphics Core", "Alice", 260000),
    Seq("Matrix Multiplication Unit", "Nina", 140000),
    Seq("Xtreme Vector Computer", "Alice", 140000)
  )

  protected def setupEmployeesTable(db: Database): Unit = {
    db.createTable(employeesTableName, employeesSchema)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
  }

  protected def setupOrganizationTable(db: Database): Unit = {
    db.createTable(organizationTableName, organizationSchema)
    organizationTable.foreach(row => db.addRow(organizationTableName, row))
  }

  protected def setupProductsTable(db: Database): Unit = {
    db.createTable(productsTableName, productsSchema)
    productsTable.foreach(row => db.addRow(productsTableName, row))
  }

  protected def setupExampleTables(db: Database): Unit = {
    setupEmployeesTable(db)
    setupOrganizationTable(db)
    setupProductsTable(db)
  }
}