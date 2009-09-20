module Main() 
    where

import System.IO
import Database.HaskellDB
import Database.HaskellDB.HSQL.SQLite3

opts = SQLiteOptions {
         filepath = "kofi.db",
         mode = ReadWriteMode
       }

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect opts

main = print ("Hello, world!!!")