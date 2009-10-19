module Queries
    where

import qualified Entity as Entity

import Database.HaskellDB

import ServerDB
import ServerDB.Company
import ServerDB.Status
import ServerDB.Account

-- lstCompanies :: Database -> IO [Entity.Company]
-- lstCompanies db = do
--   let q = do
--         cs <- table company
--         as <- table account
--         restrict (cs!unp .==. as!owner_id)        
--         project (name << cs!name # ServerDB.Account.xid << as!ServerDB.Account.xid)
--   rows <- query db q
--   mapM_ (utStrLn . \ r -> show (r!name) ++ "\t" ++ show (r!ServerDB.Account.xid)) rows
--mapM_ (putStrLn . \ r -> show (r!name) ++ "\t" ++ show (r!ServerDB.Account.xid)) rows

getCompany :: Database -> Integer -> IO Entity.Company
getCompany db local_unp = do
  print $ "hello"
  let q = do
        cs <- table company
        restrict (cs!unp .==. constant local_unp)
        project (unp << cs!unp 
                 # name << cs!name
                 # registry_date << cs!registry_date
--                 # unregistry_date << cs!unregistry_date
                 # open_key << cs!open_key)          
  print $ "good by" 
  rows <- query db q
  print $ "fetched" 
  let row = head rows 
  print $ show row
  return Entity.Company { unp = row!unp
                        , name = row!name
--                        , registryDate = row!registry_date
--                        , unregistryDate = row!unregistry_date
                        , openKey = row!open_key}
  