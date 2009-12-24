module DataModel
    where

import Data.List
import Data.Maybe
import System.Time
import Database.HSQL
import Control.Exception
import qualified System.Log.Logger as Logger

import System.IO
import Database.HSQL.SQLite3

--------------------------------------------------------------------------------
-- Logging utility functions
--------------------------------------------------------------------------------

infoM = Logger.infoM "server.db"
errorM = Logger.errorM "server.db"

--------------------------------------------------------------------------------
-- Utiliy functions
--------------------------------------------------------------------------------

catchQueries = catchSql

fromSqlMaybe t (Just v) = fromJust $ fromSqlValue t v
fromSqlMaybe _ Nothing = Nothing

fromSqlClock v = do
  let clockT = fromJust $ fromSqlValue (SqlTimeStamp) v
  calendarT <- toCalendarTime clockT
  return calendarT

fromSqlClockMaybe Nothing = return Nothing
fromSqlClockMaybe (Just v) = do
  let clockT = fromJust $ fromSqlValue (SqlTimeStamp) v
  calendarT <- toCalendarTime clockT
  return (Just calendarT)
  
formatValues values = foldl1 (++) $ intersperse ", " values

--------------------------------------------------------------------------------
-- Common functions
--------------------------------------------------------------------------------

dataBaseFilePath = "server.db"

withDB :: (Connection -> IO a) -> IO a
withDB = bracket
         (connect dataBaseFilePath ReadWriteMode)
         (\conection -> disconnect conection)

-- sqlLogErrorHandler :: SqlError -> IO a
-- sqlLogErrorHandler e = do
--   errorM (show e)  

-- sqlExec errorHandler command 

--------------------------------------------------------------------------------
-- Companies
--------------------------------------------------------------------------------

data Company = Company { unp :: String
                       , name :: String
                       , registryDate :: CalendarTime
                       , unregistryDate :: Maybe CalendarTime
                       , openKey :: String
                       }
               deriving (Read, Show)

insertCompany connection company = do
  infoM q
  execute connection q
  where q = "INSERT INTO Company VALUES (" ++ values ++");"
        values = formatValues [ toSqlValue $ unp company
                              , toSqlValue $ name company
                              , toSqlValue $ toClockTime (registryDate company)
                              , clockValue $ unregistryDate company
                              , toSqlValue $ openKey company ]
          where clockValue (Just a) = toSqlValue $ toClockTime a
                clockValue Nothing = "NULL"

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

data Account = Account { acc_id :: String
                       , bank_bic :: String
                       , owner_unp :: String
                       , ballance :: Double
                       , open_date :: CalendarTime
                       , close_date :: Maybe CalendarTime 
                       }               
               deriving (Read, Show)

insertAccount connection account = do
  infoM cmd
  execute connection cmd
  where cmd = "INSERT INTO Account VALUES(" ++ values ++");"
        values = formatValues [ toSqlValue $ acc_id account
                              , toSqlValue $ bank_bic account
                              , toSqlValue $ owner_unp account
                              , toSqlValue $ ballance account
                              , toSqlValue $ toClockTime (open_date account)
                              , clockValue $ close_date account ]
          where clockValue (Just a) = toSqlValue $ toClockTime a
                clockValue Nothing = "NULL"                 

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------
                
-- все Maybe
  
-- insertAccount
-- updateAccount

-- insertCompany
-- updateCompany

-- findCompanyByUNP
-- findAccountByACC
-- findBankByBIC
-- findCompanyByACC

-- findTransactionsForStatement ACC From To
-- findTransactionsForLog ACC From To




