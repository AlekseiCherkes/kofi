module Main()
    where

import Database.HaskellDB.DBSpec.DBSpecToDBDirect

import DBInfo
 
--------------------------------------------------------------------------------
-- Create database declaration modules.
--------------------------------------------------------------------------------

main :: IO ()
main = do 
  dbInfoToModuleFiles "Src/Server" "ServerDB" serverDBInfo

--------------------------------------------------------------------------------