module WithDB(withDB)
    where 

import System.IO
import Control.Exception
import Database.HSQL.SQLite3
-- import Database.HSQL

dataBaseFilePath = "server.db"

withDB :: (Connection -> IO a) -> IO a
withDB = bracket
         (connect dataBaseFilePath ReadWriteMode)
         (\conection -> disconnect conection)
