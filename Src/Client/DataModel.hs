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


data Bank = Bank { bnkName :: Name
                 , bnkBic  :: BIC }
            deriving(Read, Show)

data Account = Account{accNumber  :: ACC
                      ,accBank    :: BIC
                      ,accCompany :: UNP}

--------------------------------------------------------------------------------
-- !!! FIXME !!!
--------------------------------------------------------------------------------

str2unp = id

--------------------------------------------------------------------------------
-- Connection functions
--------------------------------------------------------------------------------

dataBaseFilePath = "server.db"
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
-- Access functions
--------------------------------------------------------------------------------

findCompanyByName :: Name -> IO Company
findCompanyByName name = return $ Company name (str2unp "000000001")

findCompanyByUnp  :: UNP -> IO Company
findCompanyByUnp unp   = return $ Company "Company 1" unp

findCompaniesByBank :: BIC -> IO [Company]
findCompaniesByBank bic = return []




findBankByName :: Name -> IO Bank
findBankByName name = return $ Bank name (str2bic "001")

findBankByBic :: BIC -> IO Bank
findBankByBic bic = return $ Bank "Bank 1" bic

findBanksByCompany :: UNP -> IO [Bank]
findBanksByCompany unp = return []

listBranches :: IO [Bank]
listBranches = catchSql 
               (withBanksManual perform)
               (\e -> print (show e) >> return [])
  where perform connection =  do
          let q = "SELECT * FROM Branch;"
          rows <- query connection q
          branches <- collectRows getRow rows
          print $ show branches
          return branches
          where getRow stmt = do
                  fvName <- getFieldValue stmt "name"
                  fvBic <- getFieldValue stmt "branch_bic"
                  let name = fromJust $ fromSqlValue (SqlVarChar 200) fvName :: String
                  let bic = fromJust $ fromSqlValue (SqlVarChar 9) fvBic
                  return $ (Bank name bic)

  
  -- return [Bank "bank1" (str2bic "129834765")
  --        ,Bank "bank2" (str2bic "987654321")]



findAccountsByCompany :: UNP -> IO [Account]
findAccountsByCompany unp = return []

findAccountsByBank :: BIC -> IO [Account]
findAccountsByBank bic = return []

findAccounts :: BIC -> ACC -> IO Account
findAccounts bic acc = return $ Account acc bic (str2unp "0000000001")




findCounterparties :: UNP -> IO [Company]
findCounterparties unp = return []

--------------------------------------------------------------------------------





