module WithDB(withDB)
    where 

import System.IO
import Database.HaskellDB
import Database.HaskellDB.HSQL.SQLite3

dataBaseFilePath = "server.db"

opts = SQLiteOptions { 
         filepath = dataBaseFilePath
       , mode = ReadWriteMode
       }

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect opts
