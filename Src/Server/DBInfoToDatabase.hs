module Main()
    where

import Database.HaskellDB.DBSpec.DBSpecToDatabase

import DBInfo
import WithDB
 
--------------------------------------------------------------------------------
-- Create physical database file with layout specefied in DBInfo.
--------------------------------------------------------------------------------

main :: IO ()
main = do 
  withDB (\ db -> dbSpecToDatabase db serverDBInfo)

--------------------------------------------------------------------------------