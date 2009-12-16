module DataModel where

import Validation

import System.IO
import Data.Maybe
import Data.String.UTF8
import Control.Exception
import Database.HSQL.SQLite3

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data Company = Company{cmpName :: Name
                      ,cmpUnp  :: UNP}
            deriving(Read, Show)

data Bank = Bank { bnkName :: Name
                 , bnkBic  :: BIC }
            deriving(Read, Show)

data Account = Account{accNumber  :: ACC
                      ,accBank    :: BIC
                      ,accCompany :: UNP}
            deriving(Read, Show)

--------------------------------------------------------------------------------
-- !!! FIXME !!!
--------------------------------------------------------------------------------

str2unp = id

--------------------------------------------------------------------------------
-- Connection functions
--------------------------------------------------------------------------------

dataBaseFilePath = "client.db"
banksManualFilePath = "manual.db"

withDB :: (Connection -> IO a) -> IO a
withDB = bracket
         (connect dataBaseFilePath ReadWriteMode)
         (\conection -> disconnect conection)

withBanksManual :: (Connection -> IO a) -> IO a
withBanksManual = bracket
                  (connect banksManualFilePath ReadMode)
                  (\conection -> disconnect conection)
                  
--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

sqlHandleError :: SqlError -> IO [a]
sqlHandleError e = do
  print $ "SQL Error: " ++ (show e)
  -- throwIO (ErrorCall $ "SQL Error: " ++ (show e))
  return []

sqlPerformQuery fetchRowFunction q connection = 
  print q >>
  query connection q >>=
  collectRows fetchRowFunction >>=
  return
  
sqlQuery connectFunction 
  fetchRowFunction 
  sqlQuery = catchSql 
             (connectFunction (sqlPerformQuery fetchRowFunction sqlQuery )) 
             sqlHandleError

sqlQueryGetFirst q = do
  results <- q
  return $ head results

--------------------------------------------------------------------------------
-- Fetch row functions
--------------------------------------------------------------------------------

fetchCompany :: Statement -> IO Company
fetchCompany stmt = do
  fvName <- getFieldValue stmt "company_name"
  fvUnp <- getFieldValue stmt "company_unp"
  let name = fromJust $ fromSqlValue (SqlVarChar 256) fvName
  let unp = fromJust $ fromSqlValue (SqlChar 13) fvUnp
  return $ Company name unp

fetchBankBranch :: Statement -> IO Bank
fetchBankBranch stmt = do
  fvName <- getFieldValue stmt "name"
  fvBic <- getFieldValue stmt "branch_bic"
  let name = fromJust $ fromSqlValue (SqlVarChar 200) fvName
  let bic = fromJust $ fromSqlValue (SqlVarChar 9) fvBic
  return $ Bank name bic
  
fetchAccount :: Statement -> IO Account
fetchAccount stmt = do
  fvAcc <- getFieldValue stmt "acc_id"
  fvUnp <- getFieldValue stmt "company_unp"
  fvBic <- getFieldValue stmt "bank_bic"
  let acc = fromJust $ fromSqlValue (SqlChar 13) fvAcc
  let unp = fromJust $ fromSqlValue (SqlChar 9) fvUnp
  let bic = fromJust $ fromSqlValue (SqlChar 13) fvBic
  return $ Account acc bic unp

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Access functions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Companies
--------------------------------------------------------------------------------

findCompanyByName :: Name -> IO Company
findCompanyByName name = sqlQueryGetFirst $
                         sqlQuery withDB fetchCompany  $
                         "SELECT * FROM Company " ++
                         "WHERE company_name = \"" ++ name ++ "\";"

findCompanyByUnp :: UNP -> IO Company
findCompanyByUnp unp = sqlQueryGetFirst $
                       sqlQuery withDB fetchCompany  $
                       "SELECT * FROM Company " ++
                       "WHERE company_unp = " ++ unpValue ++ ";"
                       where unpValue = toSqlValue unp

findCompaniesByBank :: BIC -> IO [Company]
findCompaniesByBank bic = sqlQuery withDB fetchCompany $
                          "SELECT Company.* " ++
                          "FROM Company " ++
                          "INNER JOIN Account on " ++
                          "Account.company_unp = Company.company_unp " ++
                          "WHERE Account.bank_bic = " ++ bicValue ++";"
                          where bicValue = toSqlValue bic
                                
--------------------------------------------------------------------------------
-- Banks
--------------------------------------------------------------------------------

findBankByName :: Name -> IO Bank
findBankByName name = sqlQueryGetFirst $
                      sqlQuery withBanksManual fetchBankBranch  $
                      "SELECT * FROM Branch " ++
                      "WHERE name = \"" ++ name ++ "\";"

findBankByBic :: BIC -> IO Bank
findBankByBic bic = sqlQueryGetFirst $
                    sqlQuery withBanksManual fetchBankBranch  $
                    "SELECT * FROM Branch " ++
                    "WHERE branch_bic = " ++ bicValue ++ ";"
                    where bicValue = toSqlValue bic

-- findBanksByCompany :: UNP -> IO [Bank]
-- findBanksByCompany unp = do

listBanks :: IO [Bank]
listBanks = sqlQuery withBanksManual fetchBankBranch 
            "SELECT * FROM Branch;"

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

findAccountsByCompany :: UNP -> IO [Account]
findAccountsByCompany unp = sqlQuery withDB fetchAccount $
                            "SELECT * FROM Account " ++
                            "WHERE company_unp = " ++ unpValue ++ ";"
                            where unpValue = toSqlValue unp

findAccountsByBank :: BIC -> IO [Account]
findAccountsByBank bic = sqlQuery withDB fetchAccount $
                         "SELECT * FROM Account " ++
                         "WHERE bank_bic = " ++ bicValue ++ ";"
                         where bicValue = toSqlValue bic

findAccount :: BIC -> ACC -> IO Account
findAccount bic acc = sqlQueryGetFirst $
                      sqlQuery withDB fetchAccount $
                      "SELECT * FROM Account " ++
                      "WHERE acc_id = " ++ accValue ++ " " ++
                      "AND bank_bic = " ++ bicValue ++ ";"
                      where accValue = toSqlValue acc
                            bicValue = toSqlValue bic

--------------------------------------------------------------------------------
-- Counterparties
--------------------------------------------------------------------------------

listCounterparties :: IO [Company]
listCounterparties = sqlQuery withDB fetchCompany $
                     "SELECT * FROM Company;"

--------------------------------------------------------------------------------