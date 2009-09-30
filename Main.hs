module Main() 
    where

import KofiDB
import KofiDB.Accounts
import KofiDB.Companies
import System.IO
import Database.HaskellDB
import Database.HaskellDB.HSQL.SQLite3

opts = SQLiteOptions {
         filepath = "kofi.db",
         mode = ReadWriteMode
       }

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect opts

lstCompanies :: Database -> IO ()
lstCompanies db = do
  let q = do
        cs <- table companies
        as <- table accounts
        restrict (cs!unp .==. as!owner_id)        
        project (name << cs!name # xid << as!xid)
  rows <- query db q
  mapM_ (putStrLn . \ r -> show (r!name) ++ "\t" ++ show (r!xid)) rows

main = withDB lstCompanies