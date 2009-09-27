module Main() 
    where

import Kofi.Persons
import System.IO
import Database.HaskellDB
import Database.HaskellDB.HSQL.SQLite3

opts = SQLiteOptions {
         filepath = "kofi.db",
         mode = ReadWriteMode
       }

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect opts

insPerson :: Database -> IO ()
insPerson db = 
   insert db persons
              ( first_name <<- Just "Gohn"
              # second_name <<- Just "Galt")

--fillDB :: Database -> IO ()
--fillDB db = insPerson db

main = withDB insPerson

--main = print ("Hello, world!!!")

-- import KofiDB
-- import System.IO
-- import Database.HaskellDB
-- import Database.HaskellDB.HSQL.SQLite3
-- import Database.HaskellDB.DBSpec.DBSpecToDatabase

-- opts = SQLiteOptions {
--          filepath = "kofi.db",
--          mode = WriteMode
--        }

-- withDB :: (Database -> IO a) -> IO a
-- withDB = sqliteConnect opts

-- listTables :: Database -> IO ()
-- listTables db = do
--   tblLst <- tables db
--   dscRes <- describe db $ head tblLst
--   print $ show tblLst
--   print $ show dscRes

-- descTbl db = do
--   return ()