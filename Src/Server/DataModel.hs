module DataModel
    where

import Types

import Data.List
import Data.Maybe
import System.Time
-- import Database.HSQL
import Database.HSQL.SQLite3
import Control.Exception
import qualified System.Log.Logger as Logger

import System.IO

--------------------------------------------------------------------------------
-- Logging utility functions
--------------------------------------------------------------------------------

infoM = Logger.infoM "root.db"
errorM = Logger.errorM "root.db"

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

withDB :: FilePath -> IOMode -> (Connection -> IO a) -> IO a
withDB path mode = bracket
                   (connect path mode)
                   (\conection -> disconnect conection)

withServerDB = withDB "server.db" ReadWriteMode
withManualDB = withDB "manual.db" ReadMode

sqlExec connector command = do
  infoM $ "Executing: " ++ command
  catchSql 
    (connector $ \conn -> execute conn command)
    (\e -> errorM $ show e)

-- sqlQueryList connector fetcher q = do
--   infoM $ "Query list: " ++ q
--   catchSql
--   (connector $ \conn -> do
--       result <- (query conn q >>= collectRows fetcher)
--       infoM "Result: " ++ (show result)
--       -- return (Just result)
--     )
--   (\e -> errorM $ show e >> return Nothing)

-- sqlQueryRec connector fetcher q = do
--   infoM $ "Query record: " ++ q
--   catchSql
--   (connector $ \conn -> do
--       result <- (query conn q >>= collectRows fetcher)
--       infoM "Result: " ++ (show result)
--       return Nothing
--       -- if (length result) == 1
--       --   then return (Just (head result))
--       --   else return Nothing
--     )
--   (\e -> errorM $ show e >> return Nothing)

--------------------------------------------------------------------------------
-- Companies
--------------------------------------------------------------------------------

data Company = Company { unp :: String
                       , name :: String
                       , registryDate :: CalendarTime
                       , unregistryDate :: Maybe CalendarTime
                       , serverRecvKey :: RSAKey
                       , serverSendKey :: RSAKey
                       , clientRecvKey :: RSAKey
                       , clientSendKey :: RSAKey
                       }
               deriving (Read, Show)

insertCompany company = sqlExec withServerDB cmd
  where cmd = "INSERT INTO Company VALUES (" ++ values ++");"
        values = formatValues [ toSqlValue $ unp company
                              , toSqlValue $ name company
                              , toSqlValue $ toClockTime (registryDate company)
                              , clockValue $ unregistryDate company
                              , toSqlValue $ show $ serverRecvKey company 
                              , toSqlValue $ show $ serverSendKey company
                              , toSqlValue $ show $ clientRecvKey company 
                              , toSqlValue $ show $ clientSendKey company ]
          where clockValue (Just a) = toSqlValue $ toClockTime a
                clockValue Nothing = "NULL"

-- findCompanyByUNP connection company = do
--   infoM q
-- sqlExec str

-- sqlQuery str

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

insertAccount account = sqlExec withServerDB cmd
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

-- insertTransaction
                
-- findTransactionsForStatement ACC From To
-- findTransactionsForLog ACC From To




