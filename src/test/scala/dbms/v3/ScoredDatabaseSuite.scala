package dbms.v3

import dbms.v3.interface.DataType.{NumericType, TextType}
import org.scalatest.funsuite.AnyFunSuite
import interface.{DataType, *}
import implementation.createNewDatabase

class ScoredDatabaseSuite extends AnyFunSuite, ExampleTables, TableComparison {

  protected def createSampleDatabase: Database = {
    val db = createNewDatabase
    setupExampleTables(db)
    db
  }

  test("Db should be empty before creating a table") {
    val db = createNewDatabase
    assert(db.listTables.isEmpty)
  }

  test("A created table should exist in the database") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    assert(db.listTables.toSet == Set(employeesTableName))
    assert(db.getColumnNames(employeesTableName) == employeesSchema.map((name, _) => name))
  }

  test("A created table should be empty") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    assert(db.getNumberOfRows(employeesTableName) == 0)
  }

  test("Creating a table with a name that already exists should fail") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    assertThrows[TableAlreadyExistsException] {
      db.createTable(employeesTableName, employeesSchema)
    }
  }

  test("Not allowed to create Table with empty schema") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    assertThrows[IllegalSchemaException] {
      db.createTable(organizationTableName, Seq.empty)
    }
  }

  test("After deleting a table it should not exist anymore and deleting a non existing table should not work") {
    val db = createNewDatabase
    db.createTable(organizationTableName, organizationSchema)
    assert(db.deleteTable(organizationTableName))
    assert(db.listTables.isEmpty)
  }

  test("Deleting a non existing table should return false") {
    val db = createNewDatabase
    assert(!db.deleteTable(organizationTableName))
  }

  test("Convert a table to CSV without header") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val expectedCsv = "\"Bob\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Mark\",3400,200,\"Marketing\"\n\"Nina\",6766,200,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n"
    assert(db.asCsv(employeesTableName, false) == expectedCsv)
  }

  test("Convert a table to CSV with header") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val expectedCsv = "\"Name\",\"Salary\",\"Bonus\",\"Department\"\n\"Bob\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Mark\",3400,200,\"Marketing\"\n\"Nina\",6766,200,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n"
    assert(db.asCsv(employeesTableName, true) == expectedCsv)
  }

  test("Convert an empty table to CSV without header") {
    val db = createNewDatabase
    db.createTable(organizationTableName, organizationSchema)
    val expectedCsv = ""
    assert(db.asCsv(organizationTableName, false) == expectedCsv)
  }

  test("Convert an empty table to CSV with header") {
    val db = createNewDatabase
    db.createTable(organizationTableName, organizationSchema)
    val expectedCsv = "\"Name\",\"Department\",\"BuildingID\",\"Floor\"\n"
    assert(db.asCsv(organizationTableName, true) == expectedCsv)
  }

  test("Non existing table to CSV should throw an exception") {
    val db = createNewDatabase
    assertThrows[NoSuchTableException] {
      db.asCsv(organizationTableName, false)
    }
    assertThrows[NoSuchTableException] {
      db.asCsv(organizationTableName, true)
    }
  }

  test("CSV should respect escaping rules") {
    val db = createNewDatabase
    db.createTable("dummy", Seq("Author" -> TextType, "Quote" -> TextType, "Year" -> NumericType))
    val testRow = Seq(Entry("Edgar \"Ed\" Roe"), Entry("I said \"Stop this\" and left."), Entry(2020))
    db.addRow("dummy", testRow)
    assert(db.getRow("dummy", 0) == testRow)
    assert(db.getColumnNames("dummy") == Seq("Author", "Quote", "Year"))
    val expectedCsv = "\"Author\",\"Quote\",\"Year\"\n\"Edgar \"\"Ed\"\" Roe\",\"I said \"\"Stop this\"\" and left.\",2020\n"
    assert(db.asCsv("dummy", true) == expectedCsv)
  }

  test("CSV headers should also respect escaping rules") {
    val db = createNewDatabase
    db.createTable("dummy", Seq("Author (\"Writer Boy\")" -> TextType, "Quote (\"Say-thing\")" -> TextType))
    val expectedCsv = "\"Author (\"\"Writer Boy\"\")\",\"Quote (\"\"Say-thing\"\")\"\n"
    assert(db.getColumnNames("dummy") == Seq("Author (\"Writer Boy\")", "Quote (\"Say-thing\")"))
    assert(db.asCsv("dummy", true) == expectedCsv)
  }

  test("Should throw an exception when adding a row to a non existing table") {
    val db = createNewDatabase
    val row = employeesTable.head
    assertThrows[NoSuchTableException] {
      db.addRow(employeesTableName, row)
    }
  }

  test("Should throw an exception when adding a row with mismatched count") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    val row = employeesTable.head.drop(1)
    assertThrows[DataTypeMismatchException] {
      db.addRow(employeesTableName, row)
    }
  }

  // IntType Test
  test("Should throw an exception when adding a row with mismatched schema") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    val row = employeesTable.head.updated(0, Entry(1))
    assertThrows[DataTypeMismatchException] {
      db.addRow(employeesTableName, row)
    }
  }

  test("Should add the right row to an existing table") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    val row = employeesTable.head
    db.addRow(employeesTableName, row)
    assert(isRowCorrect(employeesSchema, db.getRow(employeesTableName, 0), row))
  }

  test("Should add multiple rows correctly") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
    val expectedCSV = "\"Name\",\"Salary\",\"Bonus\",\"Department\"\n\"Bob\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Mark\",3400,200,\"Marketing\"\n\"Nina\",6766,200,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n"
    assert(db.asCsv(employeesTableName, true) == expectedCSV)
  }

  test("Should add multiple rows correctly after deleting and adding the table again") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
    db.deleteTable(employeesTableName)
    db.createTable(employeesTableName, employeesSchema)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
    val expectedCSV = "\"Name\",\"Salary\",\"Bonus\",\"Department\"\n\"Bob\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Mark\",3400,200,\"Marketing\"\n\"Nina\",6766,200,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n"
    assert(db.asCsv(employeesTableName, true) == expectedCSV)
  }

  test("Should return the correct column names of an existing table") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    assert(db.getColumnNames(employeesTableName) == employeesSchema.map(_._1))
  }

  test("Should return the correct column names of an existing table with added rows") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
    assert(db.getColumnNames(employeesTableName) == employeesSchema.map(_._1))
  }

  test("Should return the correct column names of an existing table after deleting and adding with another schema") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
    db.deleteTable(employeesTableName)
    db.createTable(employeesTableName, organizationSchema)
    assert(db.getColumnNames(employeesTableName) == organizationSchema.map(_._1))
  }

  test("should throw an exception when getting column names of a non-existing table") {
    val db = createNewDatabase
    assertThrows[NoSuchTableException] {
      db.getColumnNames("Non existing table")
    }
  }

  test("should throw an exception when getting column names of a non-existing table after adding, filling and then deleting") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
    db.deleteTable(employeesTableName)
    assertThrows[NoSuchTableException] {
      db.getColumnNames("Non existing table")
    }
  }

  test("should return data types of columns of an existing table") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val dataTypes = db.getDataTypes(employeesTableName)
    assert(dataTypes == employeesSchema.map(_._2))
  }

  test("should return data types of columns of an existing table after adding rows") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
    val dataTypes = db.getDataTypes(employeesTableName)
    assert(dataTypes == employeesSchema.map(_._2))
  }

  test("Should throw an exception when getting data types of columns of a non existing table") {
    val db = createNewDatabase
    assertThrows[NoSuchTableException] {
      db.getDataTypes("Non existing table")
    }
  }

  test("Should throw an exception when getting data types of columns of a non existing table after filling and deleting") {
    val db = createNewDatabase
    assertThrows[NoSuchTableException] {
      db.getDataTypes("Non existing table")
    }
  }

  test("Should return correct datatypes after deleting and adding again with another schema") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    employeesTable.foreach(row => db.addRow(employeesTableName, row))
    db.deleteTable(employeesTableName)
    db.createTable(employeesTableName, organizationSchema)
    val dataTypes = db.getDataTypes(employeesTableName)
    assert(dataTypes == organizationSchema.map(_._2))
  }

  test("Should return the number of rows of an existing table") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    assert(db.getNumberOfRows(employeesTableName) == employeesTable.size)
  }

  test("Should always return the right number of rows") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    assert(db.getNumberOfRows(employeesTableName) == 0)
    db.addRow(employeesTableName, employeesTable(0))
    assert(db.getNumberOfRows(employeesTableName) == 1)
    db.addRow(employeesTableName, employeesTable(1))
    assert(db.getNumberOfRows(employeesTableName) == 2)
    db.addRow(employeesTableName, employeesTable(3))
    assert(db.getNumberOfRows(employeesTableName) == 3)
  }

  test("Should throw an exception when getting the number of rows of a non existing table") {
    val db = createNewDatabase
    assertThrows[NoSuchTableException] {
      db.getNumberOfRows("Non existing table")
    }
  }

  test("Should throw an exception when getting the number of rows of a non existing table which was added and deleted") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.deleteTable(employeesTableName)
    assertThrows[NoSuchTableException] {
      db.getNumberOfRows("Non existing table")
    }
  }

  test("Should return a row of an existing table") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val row = db.getRow(employeesTableName, 1)
    assert(row == employeesTable(1))
  }

  test("Should always return the right row") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    for (x <- 0 until db.getNumberOfRows(employeesTableName)) {
      val row = db.getRow(employeesTableName, x)
      assert(row == employeesTable(x))
    }
  }

  test("Exception when getting a row from non existing table") {
    val db = createNewDatabase
    assertThrows[NoSuchTableException] {
      db.getRow("Non existing table", 3)
    }
  }

  test("Should throw an exception when getting a row with too big offset") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    assertThrows[IndexOutOfBoundsException] {
      db.getRow(employeesTableName, 20)
    }
  }

  test("Should throw an exception when getting a row with too small offset") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    assertThrows[IndexOutOfBoundsException] {
      db.getRow(employeesTableName, -1)
    }
  }

  test("Combined test for interface") {
    val db = createNewDatabase
    setupExampleTables(db)
    db.deleteTable(organizationTableName)
    db.addRow(employeesTableName, employeesTable(0))
    val expectedCSV = "\"Name\",\"Salary\",\"Bonus\",\"Department\"\n\"Bob\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Mark\",3400,200,\"Marketing\"\n\"Nina\",6766,200,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n\"Bob\",3400,320,\"Marketing\"\n"
    assert(expectedCSV == db.asCsv(employeesTableName, true))
    val dataTypes = db.getDataTypes(employeesTableName)
    assert(dataTypes == employeesSchema.map(_._2))
    assert(db.getRow(employeesTableName, 0) == db.getRow(employeesTableName, db.getNumberOfRows(employeesTableName) - 1))
  }

  test("After running a correct query the resulting table should exist") {
    val db = createNewDatabase
    setupExampleTables(db)
    val query = ReadTable(employeesTableName)
    val resultName = "New Employees"
    db.runQuery(resultName, query)

    assert(db.listTables.exists(_ == resultName))
  }

  test("Query should create Table with same scheme") {
    val db = createNewDatabase
    setupExampleTables(db)
    val queryPlan = ReadTable(employeesTableName)
    db.runQuery("ResultTable", queryPlan)
    val resultColumns = db.getColumnNames("ResultTable")
    assert(resultColumns == db.getColumnNames(employeesTableName))
  }

  test("A Table query should result in a table identical to the named one") {
    val db = createSampleDatabase
    val query = ReadTable(employeesTableName)
    val resultName = "New Employees"
    db.runQuery(resultName, query)
    assert(isTableCorrect(db, resultName, employeesSchema, employeesTable))
  }

  test("Query should create Table with same values") {
    val db = createNewDatabase
    setupExampleTables(db)
    val queryPlan = ReadTable(employeesTableName)
    db.runQuery("ResultTable", queryPlan)
    assert(db.asCsv("ResultTable", false) == db.asCsv(employeesTableName, false))
  }

  test("Throw an exception when reading non existing table") {
    val db = createNewDatabase
    setupExampleTables(db)
    val queryPlan = ReadTable("Non existing Table")
    assertThrows[NoSuchTableException] {
      db.runQuery("Test", queryPlan)
    }
  }

  test("Rename Column should rename an existing column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryPlan = RenameColumn("Salary", "Payment", ReadTable(employeesTableName))
    db.runQuery("result", queryPlan)
    val resultColumns = db.getColumnNames("result")
    assert(resultColumns == employeesSchema.map(_._1).updated(1, "Payment"))
  }

  test("Rename Column should throw an exception when renaming non existing column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryPlan = RenameColumn("NonExisting", "Else", ReadTable(employeesTableName))
    assertThrows[NoSuchColumnException] {
      db.runQuery("result", queryPlan)
    }
  }

  test("Throw an exception when renaming name already exists") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryPlan = RenameColumn("Name", "Salary", ReadTable(employeesTableName))
    assertThrows[ColumnAlreadyExistsException] {
      db.runQuery("result", queryPlan)
    }
  }

  test("Throw an exception when renaming name already exists with same name") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryPlan = RenameColumn("Name", "Name", ReadTable(employeesTableName))
    assertThrows[ColumnAlreadyExistsException] {
      db.runQuery("result", queryPlan)
    }
  }

  test("Throw an exception when renaming an column that was there before but is not anymore") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryPlan = RenameColumn("Name", "Namee", ReadTable(employeesTableName))
    db.runQuery("result", queryPlan)
    val queryPlan2 = RenameColumn("Name", "OtherName", ReadTable("result"))
    assertThrows[NoSuchColumnException] {
      db.runQuery("result2", queryPlan2)
    }
  }

  test("A rename query should result in all rows of the original table") {
    val db = createSampleDatabase
    val toRename = "Bonus"
    val newName = "Additional Salary"
    val query = RenameColumn(toRename, newName, ReadTable(employeesTableName))
    val resultName = "Employees 2"
    db.runQuery(resultName, query)
    assert(isTableCorrect(db, resultName, renamedEmployeesSchema, employeesTable))
  }

  test("PickColumns should select a subset of columns") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = PickColumns(Seq("Name", "Bonus"), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    assert(db.getColumnNames("Result") == Seq("Name", "Bonus"))
  }

  test("PickColumns should select a subset of columns in the right order") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = PickColumns(Seq("Bonus", "Name"), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    assert(db.getColumnNames("Result") == Seq("Bonus", "Name"))
  }

  test("PickColumns should have the same entries in the table as before") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = PickColumns(Seq("Bonus"), ReadTable(employeesTableName))
    val resultName = "Result"
    db.runQuery(resultName, query)
    assert(isTableCorrect(db, resultName, Seq("Bonus" -> DataType.NumericType), employeesTable.map(row => Seq(row(2)))))
  }

  test("PickColumns should throw an exception when picking an empty subset of columns") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = PickColumns(Seq.empty, ReadTable(employeesTableName))
    assertThrows[IllegalSchemaException] {
      db.runQuery("Result", query)
    }
  }

  test("PickColumns should throw an exception when picking a non existing column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = PickColumns(Seq("Non existing column"), ReadTable(employeesTableName))
    assertThrows[NoSuchColumnException] {
      db.runQuery("Result", query)
    }
  }

  test("PickColumns and RenameColumn should work together") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = RenameColumn("Name", "NewName", PickColumns(Seq("Name", "Bonus"), ReadTable(employeesTableName)))
    db.runQuery("Result", query)
    assert(db.getColumnNames("Result") == Seq("NewName", "Bonus"))
    assert(db.getNumberOfRows("Result") == db.getNumberOfRows(employeesTableName))
  }

  test("PickColumns and RenameColumn should work together more advanced") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = PickColumns(Seq("NewName"), RenameColumn("Name", "NewName", PickColumns(Seq("Name", "Bonus"), ReadTable(employeesTableName))))
    db.runQuery("Result", query)
    assert(db.getColumnNames("Result") == Seq("NewName"))
  }

  test("RemoveDuplicates should remove duplicates") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(2))
    val query = RemoveDuplicates(ReadTable(employeesTableName))
    db.runQuery("Result", query)
    assert(isTableCorrect(db, "Result", employeesSchema, employeesTable))
  }

  test("RemoveDuplicates should also remove 'driplicates'") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    val query = RemoveDuplicates(ReadTable(employeesTableName))
    db.runQuery("Result", query)
    assert(isTableCorrect(db, "Result", employeesSchema, employeesTable))
  }

  test("RemoveDuplicates should also work when there are multiple different duplicates") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = RemoveDuplicates(ReadTable(employeesTableName))
    db.runQuery("Result", query)
    assert(isTableCorrect(db, "Result", employeesSchema, employeesTable))
  }

  test("RemoveDuplicates should work together with PickColumns and RenameColumn") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = RemoveDuplicates(RenameColumn("Name", "NewName", PickColumns(Seq("Name", "Bonus"), ReadTable(employeesTableName))))
    db.runQuery("Result", query)
    val expected = "\"NewName\",\"Bonus\"\n\"Bob\",320\n\"Alice\",600\n\"Clarissa\",1000\n\"Mark\",200\n\"Nina\",200\n\"Phillip\",600\n\"Jeff\",18014398509481994\n"
    assert(db.asCsv("Result", true) == expected)
  }

  test("FilterEqual should only retain rows with the value matches StringType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterEqual("Department", Entry("Marketing"), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.filter(row => row.contains(Entry("Marketing")))
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("FilterEqual should only retain rows with the value matches IntType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterEqual("Bonus", Entry(600), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.filter(row => row.contains(Entry(600)))
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("FilterEqual should throw an exception when filtering a non existing column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterEqual("Non existing", Entry("Marketing"), ReadTable(employeesTableName))
    assertThrows[NoSuchColumnException] {
      db.runQuery("Result", query)
    }
  }

  test("FilterEqual should return empty table if no entry matches") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterEqual("Department", Entry("Non Existing"), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.filter(s => s.contains(Entry("Non Existing")))
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("FilterEqual should work together with RemoveDuplicates, PickColumns and RenameColumn") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = FilterEqual("Department", Entry("Engineering"), RemoveDuplicates(RenameColumn("Name", "NewName", PickColumns(Seq("Name", "Department"), ReadTable(employeesTableName)))))
    db.runQuery("Result", query)
    val expected = "\"NewName\",\"Department\"\n\"Alice\",\"Engineering\"\n\"Nina\",\"Engineering\"\n"
    assert(db.asCsv("Result", true) == expected)
  }

  test("FilterSmaller should work correctly with StringType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterSmaller("Name", Entry("Clarissa"), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.filter(row => extractText(row.head).get <= "Clarissa")
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("FilterSmaller should work correctly with IntType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterSmaller("Bonus", Entry(600), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.filter(row => extractNumber(row(2)).get <= 600)
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("FilterSmaller should work together with RemoveDuplicates, PickColumns and RenameColumn") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = FilterSmaller("Department", Entry("Finance"), RemoveDuplicates(RenameColumn("Name", "NewName", PickColumns(Seq("Name", "Department"), ReadTable(employeesTableName)))))
    db.runQuery("Result", query)
    val expected = "\"NewName\",\"Department\"\n\"Alice\",\"Engineering\"\n\"Nina\",\"Engineering\"\n\"Phillip\",\"Finance\"\n\"Jeff\",\"CEO\"\n"
    assert(db.asCsv("Result", true) == expected)
  }

  test("FilterSmaller should throw an exception if column is non existing") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterSmaller("Non existing column", Entry("D"), ReadTable(employeesTableName))
    assertThrows[NoSuchColumnException] {
      db.runQuery("ResultTable", query)
    }
  }

  test("FilterSmaller should throw an exception if wrong datatype") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterSmaller("Name", Entry(10), ReadTable(employeesTableName))
    assertThrows[DataTypeMismatchException] {
      db.runQuery("ResultTable", query)
    }
  }

  test("FilterGreater should work correctly with StringType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterGreater("Name", Entry("Clarissa"), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.filter(row => extractText(row.head).get >= "Clarissa")
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("FilterGreater should work correctly with IntType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterGreater("Bonus", Entry(600), ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.filter(row => extractNumber(row(2)).get >= 600)
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("FilterGreater together with FilterSmaller should be the same result as FilterEqual") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryGS = FilterSmaller("Name", Entry("Clarissa"), FilterGreater("Name", Entry("Clarissa"), ReadTable(employeesTableName)))
    db.runQuery("Result1", queryGS)
    val queryE = FilterEqual("Name", Entry("Clarissa"), ReadTable(employeesTableName))
    db.runQuery("Result2", queryE)
    assert(db.asCsv("Result1", true) == db.asCsv("Result2", true))
  }

  test("FilterGreater should work together with RemoveDuplicates, PickColumns and RenameColumn") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = FilterGreater("Department", Entry("Finance"), RemoveDuplicates(RenameColumn("Name", "NewName", PickColumns(Seq("Name", "Department"), ReadTable(employeesTableName)))))
    db.runQuery("Result", query)
    val expected = "\"NewName\",\"Department\"\n\"Bob\",\"Marketing\"\n\"Clarissa\",\"Management\"\n\"Mark\",\"Marketing\"\n\"Phillip\",\"Finance\"\n"
    assert( db.asCsv("Result", true) == expected)
  }

  test("FilterGreater should throw an exception when filtering on a non existing column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterGreater("Non existing column", Entry("D"), ReadTable(employeesTableName))
    assertThrows[NoSuchColumnException] {
      db.runQuery("ResultTable", query)
    }
  }

  test("FilterGreater should throw an exception if wrong datatype") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = FilterGreater("Name", Entry(10), ReadTable(employeesTableName))
    assertThrows[DataTypeMismatchException] {
      db.runQuery("ResultTable", query)
    }
  }

  test("Sort should sort table in ascending order with StringType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = Sort("Name", false, ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.sortBy(seq => extractText(seq.head))
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("Sort should sort table in ascending order with IntType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = Sort("Bonus", false, ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.sortBy(seq => extractNumber(seq(2)))
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("Sort should sort table in descending order with StringType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = Sort("Name", true, ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.sortBy(seq => extractText(seq.head).get)(Ordering[String].reverse)
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("Sort should sort table in descending order with IntType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = Sort("Bonus", true, ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expectedTable = employeesTable.sortBy(seq => extractNumber(seq(2)).get)(Ordering[Long].reverse)
    assert(isTableCorrect(db, "Result", employeesSchema, expectedTable))
  }

  test("Sort should work if table has duplicates IntType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = Sort("Bonus", true, ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expected = "\"Name\",\"Salary\",\"Bonus\",\"Department\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Alice\",6766,600,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\"\n\"Alice\",6766,600,\"Engineering\"\n\"Bob\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Mark\",3400,200,\"Marketing\"\n\"Nina\",6766,200,\"Engineering\"\n"
    assert(expected == db.asCsv("Result", true))
  }

  test("Sort should work if table has duplicates StringType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = Sort("Name", true, ReadTable(employeesTableName))
    db.runQuery("Result", query)
    val expected = "\"Name\",\"Salary\",\"Bonus\",\"Department\"\n\"Phillip\",5400,600,\"Finance\"\n\"Nina\",6766,200,\"Engineering\"\n\"Mark\",3400,200,\"Marketing\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Bob\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\"\n\"Alice\",6766,600,\"Engineering\"\n"
    assert(db.asCsv("Result", true) == expected)
  }

  test("Sort should work multiple times on different Columns StringType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = Sort("Department", false, Sort("Name", true, ReadTable(employeesTableName)))
    db.runQuery("Result", query)
    val expected = "\"Name\",\"Salary\",\"Bonus\",\"Department\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n\"Nina\",6766,200,\"Engineering\"\n\"Alice\",6766,600,\"Engineering\"\n\"Alice\",6766,600,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Mark\",3400,200,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n"
    assert(expected == db.asCsv("Result", true))
  }

  test("Sort should work multiple times on different Columns IntType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = Sort("Bonus", false, Sort("Salary", true, ReadTable(employeesTableName)))
    db.runQuery("Result", query)
    val expected = "\"Name\",\"Salary\",\"Bonus\",\"Department\"\n\"Nina\",6766,200,\"Engineering\"\n\"Mark\",3400,200,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\"\n\"Alice\",6766,600,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\"\n\"Clarissa\",10000,1000,\"Management\"\n\"Jeff\",15000,18014398509481994,\"CEO\"\n"
    assert(expected == db.asCsv("Result", true))
  }

  test("Sort should work together with the other Queries") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    val query = Sort("NewName", true, FilterGreater("Department", Entry("Finance"), RemoveDuplicates(RenameColumn("Name", "NewName", PickColumns(Seq("Name", "Department"), ReadTable(employeesTableName))))))
    db.runQuery("Result", query)
    val expected = "\"NewName\",\"Department\"\n\"Phillip\",\"Finance\"\n\"Mark\",\"Marketing\"\n\"Clarissa\",\"Management\"\n\"Bob\",\"Marketing\"\n"
    assert(db.asCsv("Result", true) == expected)
  }

  test("Sort should throw an exception when sorting by a non existing column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val query = Sort("Non existing column", true, ReadTable(employeesTableName))
    assertThrows[NoSuchColumnException] {
      db.runQuery("ResultTable", query)
    }
  }

  test("NaturalJoin should join two tables on a shared column StringType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    var queryPlan = RenameColumn("Salary", "Age", ReadTable(employeesTableName))
    queryPlan = RenameColumn("Bonus", "Ager", queryPlan)
    queryPlan = RenameColumn("Department", "Home", queryPlan)
    db.runQuery("JoinTable", queryPlan)

    val joinQuery = NaturalJoin(queryPlan, ReadTable(employeesTableName))
    db.runQuery("JoinResult", joinQuery)
    val expected = "\"Name\",\"Age\",\"Ager\",\"Home\",\"Salary\",\"Bonus\",\"Department\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\",6766,600,\"Engineering\"\n\"Clarissa\",10000,1000,\"Management\",10000,1000,\"Management\"\n\"Mark\",3400,200,\"Marketing\",3400,200,\"Marketing\"\n\"Nina\",6766,200,\"Engineering\",6766,200,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\",5400,600,\"Finance\"\n\"Jeff\",15000,18014398509481994,\"CEO\",15000,18014398509481994,\"CEO\"\n"
    assert(db.asCsv("JoinResult", true) == expected)
  }

  test("NaturalJoin should join two tables on a shared column IntType") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    setupOrganizationTable(db)
    val queryPlan = RenameColumn("Department", "OrganizationDepartment", RenameColumn("Name", "OrganizationName", RenameColumn("BuildingID", "Salary", ReadTable(organizationTableName))))
    db.runQuery("JoinTable", queryPlan)

    val joinQuery = NaturalJoin(ReadTable("JoinTable"), ReadTable(employeesTableName))
    db.runQuery("JoinResult", joinQuery)
    val expected = "\"OrganizationName\",\"OrganizationDepartment\",\"Salary\",\"Floor\",\"Name\",\"Bonus\",\"Department\"\n\"Aldi\",\"Management\",3400,5,\"Bob\",320,\"Marketing\"\n\"Aldi\",\"Management\",3400,5,\"Mark\",200,\"Marketing\"\n\"Rewe\",\"Marketing\",6766,2,\"Alice\",600,\"Engineering\"\n\"Rewe\",\"Marketing\",6766,2,\"Nina\",200,\"Engineering\"\n"
    assert(db.asCsv("JoinResult", true) == expected)
  }

  test("NaturalJoin should join three tables on a shared column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    setupOrganizationTable(db)
    var queryPlan1 = RenameColumn("Salary", "Age", ReadTable(employeesTableName))
    queryPlan1 = RenameColumn("Bonus", "Ager", queryPlan1)
    queryPlan1 = RenameColumn("Department", "Home", queryPlan1)
    val queryPlan2 = RenameColumn("Department", "OrganizationDepartment", ReadTable(organizationTableName))
    val joinQuery = NaturalJoin(queryPlan2, NaturalJoin(queryPlan1, ReadTable(employeesTableName)))
    db.runQuery("JoinResult", joinQuery)
    val expected = "\"Name\",\"OrganizationDepartment\",\"BuildingID\",\"Floor\",\"Age\",\"Ager\",\"Home\",\"Salary\",\"Bonus\",\"Department\"\n\"Phillip\",\"Finance\",0,3,5400,600,\"Finance\",5400,600,\"Finance\"\n"
    assert(db.asCsv("JoinResult", true) == expected)
  }

  test("NaturalJoin should also work when there is only one column in both tables") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    setupOrganizationTable(db)
    val queryPlan = PickColumns(Seq("Salary"), RenameColumn("Department", "OrganizationDepartment", RenameColumn("Name", "OrganizationName", RenameColumn("BuildingID", "Salary", ReadTable(organizationTableName)))))
    db.runQuery("JoinTable", queryPlan)

    val joinQuery = NaturalJoin(ReadTable("JoinTable"), PickColumns(Seq("Salary"), ReadTable(employeesTableName)))
    db.runQuery("JoinResult", joinQuery)
    val expected = "\"Salary\"\n3400\n3400\n6766\n6766\n"
    assert(db.asCsv("JoinResult", true) == expected)
  }

  test("NaturalJoin should work correctly with Duplicates in one Table") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    var queryPlan = RenameColumn("Salary", "Age", ReadTable(employeesTableName))
    queryPlan = RenameColumn("Bonus", "Ager", queryPlan)
    queryPlan = RenameColumn("Department", "Home", queryPlan)
    db.runQuery("JoinTable", queryPlan)

    val joinQuery = NaturalJoin(queryPlan, ReadTable(employeesTableName))
    db.runQuery("JoinResult", joinQuery)
    val expected = "\"Name\",\"Age\",\"Ager\",\"Home\",\"Salary\",\"Bonus\",\"Department\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\",6766,600,\"Engineering\"\n\"Alice\",6766,600,\"Engineering\",6766,600,\"Engineering\"\n\"Clarissa\",10000,1000,\"Management\",10000,1000,\"Management\"\n\"Mark\",3400,200,\"Marketing\",3400,200,\"Marketing\"\n\"Nina\",6766,200,\"Engineering\",6766,200,\"Engineering\"\n\"Phillip\",5400,600,\"Finance\",5400,600,\"Finance\"\n\"Jeff\",15000,18014398509481994,\"CEO\",15000,18014398509481994,\"CEO\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Alice\",6766,600,\"Engineering\",6766,600,\"Engineering\"\n\"Alice\",6766,600,\"Engineering\",6766,600,\"Engineering\"\n"
    assert(db.asCsv("JoinResult", true) == expected)
  }

  test("NaturalJoin should work together with FilterEqual, Sort") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    var queryPlan = RenameColumn("Salary", "Age", ReadTable(employeesTableName))
    queryPlan = RenameColumn("Bonus", "Ager", queryPlan)
    queryPlan = RenameColumn("Department", "Home", queryPlan)

    val joinQuery = FilterEqual("Name", Entry("Bob"), NaturalJoin(queryPlan, Sort("Name", true, ReadTable(employeesTableName))))
    db.runQuery("JoinResult", joinQuery)
    val expected = "\"Name\",\"Age\",\"Ager\",\"Home\",\"Salary\",\"Bonus\",\"Department\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n"
    assert(db.asCsv("JoinResult", true) == expected)
  }

  test("NaturalJoin with should throw an exception if tables share more than 1 column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val joinQuery = NaturalJoin(ReadTable(employeesTableName), ReadTable(employeesTableName))
    assertThrows[IllegalSchemaException] {
      db.runQuery("Result", joinQuery)
    }
  }

  test("NaturalJoin should throw an exception when joining tables with no shared column") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    setupProductsTable(db)
    val joinQuery = NaturalJoin(ReadTable(employeesTableName), ReadTable(productsTableName))

    assertThrows[IllegalSchemaException] {
      db.runQuery("Result", joinQuery)
    }
  }

  test("NaturalJoin should throw an exception if the shared column is not of the same data type") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.createTable("Employees 2", employeesSchema.updated(0, "Name" -> DataType.NumericType))

    val joinQuery = NaturalJoin(ReadTable(employeesTableName), ReadTable("Employees 2"))

    assertThrows[IllegalSchemaException] {
      db.runQuery("Result", joinQuery)
    }
  }

  test("TakeFirst should retain the first few rows in the table") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryPlan = TakeFirst(2, ReadTable(employeesTableName))
    db.runQuery("Result", queryPlan)
    assert(isTableCorrect(db, "Result", employeesSchema, employeesTable.take(2)))
  }

  test("TakeFirst result should be empty if the table is empty") {
    val db = createNewDatabase
    db.createTable(employeesTableName, employeesSchema)
    val queryPlan = TakeFirst(2, ReadTable(employeesTableName))
    db.runQuery("Result", queryPlan)
    assert(isTableCorrect(db, "Result", employeesSchema, IndexedSeq.empty[Seq[Entry]]))
  }

  test("TakeFirst result should return the right amount of rows if amount is bigger than table") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryPlan = TakeFirst(20, ReadTable(employeesTableName))
    db.runQuery("Result", queryPlan)
    assert(isTableCorrect(db, "Result", employeesSchema, employeesTable.take(20)))
  }

  test("TakeFirst throw an IllegalArgumentException if the amount is negative") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    val queryPlan = TakeFirst(-1, ReadTable(employeesTableName))
    assertThrows[IllegalArgumentException] {
      db.runQuery("Result", queryPlan)
    }
  }

  test("TakeFirst should work together with NaturalJoin, FilterEqual, Sort") {
    val db = createNewDatabase
    setupEmployeesTable(db)
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(0))
    db.addRow(employeesTableName, employeesTable(1))
    var queryPlan = RenameColumn("Salary", "Age", ReadTable(employeesTableName))
    queryPlan = RenameColumn("Bonus", "Ager", queryPlan)
    queryPlan = RenameColumn("Department", "Home", queryPlan)

    val joinQuery = TakeFirst(3, FilterEqual("Name", Entry("Bob"), NaturalJoin(queryPlan, Sort("Name", true, ReadTable(employeesTableName)))))
    db.runQuery("JoinResult", joinQuery)
    val expected = "\"Name\",\"Age\",\"Ager\",\"Home\",\"Salary\",\"Bonus\",\"Department\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n\"Bob\",3400,320,\"Marketing\",3400,320,\"Marketing\"\n"
    assert(db.asCsv("JoinResult", true) == expected)
  }
}