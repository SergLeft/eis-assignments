package dbms.v3.implementation

import dbms.v3.interface.Database

/** Creates a new object satisfying the [[Database]] interface. */
def createNewDatabase: Database = DatabaseImpl()
