module DataModel where

import Types
import Message
import ClientEntities

import System.IO
import Data.Maybe
import Data.List
import Data.String.UTF8 ()
import Control.Exception
import Database.HSQL.SQLite3

--------------------------------------------------------------------------------
-- Connection functions
--------------------------------------------------------------------------------

banksManualFilePath = "manual.db"

withDB :: FilePath -> (Connection -> IO a) -> IO a
withDB filePath = bracket
                  (connect filePath ReadWriteMode)
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
  let payerAcc = Account (AccountPK acc bic) unp
  return payerAcc

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Access functions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Companies
--------------------------------------------------------------------------------

findCompanyByName :: FilePath -> Name -> IO Company
findCompanyByName file name = sqlQueryGetFirst $
                            sqlQuery (withDB file) fetchCompany  $
                            "SELECT * FROM Company " ++
                            "WHERE company_name = \"" ++ name ++ "\";"

findCompanyByUnp :: FilePath -> UNP -> IO Company
findCompanyByUnp file unp = sqlQueryGetFirst $
                          sqlQuery (withDB file) fetchCompany  $
                          "SELECT * FROM Company " ++
                          "WHERE company_unp = " ++ unpValue ++ ";"
                            where unpValue = toSqlValue unp

findCompaniesByBank :: FilePath -> BIC -> IO [Company]
findCompaniesByBank file bic = sqlQuery (withDB file) fetchCompany $
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

-- not tested yet !!!
findBanksByCompany :: FilePath -> UNP -> IO [Bank]
findBanksByCompany file unp = do
  accounts <- fetchAccountsByCompany
  banks <- fetchBanksByBics $ map (bankBic . accPk) accounts
  return banks
  where 
    fetchAccountsByCompany = sqlQuery (withDB file) fetchAccount $
                             "SELECT Account.bank_bic " ++  
                             "FROM Company " ++ 
                             "INNER JOIN Account ON Account.company_unp = Company.company_unp " ++
                             "WHERE Company.company_unp = " ++ unpValue ++ ";"
                               where unpValue = toSqlValue unp
                 
    fetchBanksByBics bics = sqlQuery withBanksManual fetchBankBranch $
                            "SELECT * " ++ 
                            "FROM Branch " ++
                            "WHERE branch_bic IN (" ++ bicsValue ++ ");"
                              where bicsValue = foldl1 (++) $ intersperse ", " $ 
                                                map toSqlValue bics

listBanks :: IO [Bank]
listBanks = sqlQuery withBanksManual fetchBankBranch 
            "SELECT * FROM Branch;"

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

findAccountsByCompany :: FilePath -> UNP -> IO [Account]
findAccountsByCompany file unp = sqlQuery (withDB file) fetchAccount $
                                 "SELECT * FROM Account " ++
                                 "WHERE company_unp = " ++ unpValue ++ ";"
                                   where unpValue = toSqlValue unp

findAccountsByBank :: FilePath -> BIC -> IO [Account]
findAccountsByBank file bic = sqlQuery (withDB file) fetchAccount $
                              "SELECT * FROM Account " ++
                              "WHERE bank_bic = " ++ bicValue ++ ";"
                                where bicValue = toSqlValue bic

findAccount :: FilePath -> BIC -> ACC -> IO Account
findAccount file bic acc = sqlQueryGetFirst $
                           sqlQuery (withDB file) fetchAccount $
                           "SELECT * FROM Account " ++
                           "WHERE acc_id = " ++ accValue ++ " " ++
                           "AND bank_bic = " ++ bicValue ++ ";"
                             where accValue = toSqlValue acc
                                   bicValue = toSqlValue bic

--------------------------------------------------------------------------------
-- Counterparties
--------------------------------------------------------------------------------

listCounterparties :: FilePath -> IO [Company]
listCounterparties file = sqlQuery (withDB file) fetchCompany $
                          "SELECT * FROM Company;"

--------------------------------------------------------------------------------