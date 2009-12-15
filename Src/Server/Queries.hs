module Queries
    where

import Entity

import Data.List
import Data.Maybe
import System.Time
import Database.HSQL -- .Types
import Control.Exception
import qualified System.Log.Logger as Logger

--------------------------------------------------------------------------------
-- Logging utility functions
--------------------------------------------------------------------------------

infoM = Logger.infoM "server.db"
errorM = Logger.errorM "server.db"

--------------------------------------------------------------------------------
-- Common functions
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

--------------------------------------------------------------------------------
-- Database functions
--------------------------------------------------------------------------------

insertCompany connection company = do
  infoM q
  execute connection q
  where q = "INSERT INTO Company VALUES (" ++ values ++");"
        values = foldl (++) "" $ intersperse ", " [toSqlValue $ unp company, 
                                                   toSqlValue $ name company, 
                                                   toSqlValue $ toClockTime (registryDate company), 
                                                   clockValue $ unregistryDate company,
                                                   toSqlValue $ openKey company]
          where clockValue (Just a) = toSqlValue $ toClockTime a
                clockValue Nothing = "NULL"

-- findCompany connection unp = do
--   infoM q
--   company <- bracket (query connection q) (closeStatement) fetchRow
--   return company
--   where q = "SELECT * FROM Company WHERE company_unp = " ++ value ++ ";"
--         value = toSqlValue unp
--         fetchRow s = do
--           fetch s -- skip header
          
--           fvUnp <- getFieldValue s "company_unp"
--           fvName <- getFieldValue s "name" 
--           fvRegDate <- getFieldValue s "registry_date"
--           -- fvUnregDate <- getFieldValueMB s "unregistry_date"
--           -- fvKey <- getFieldValue s "open_key"
          
--           let unp = fromJust $ fromSqlValue (SqlChar 13) fvUnp
--           let name = fromJust $ fromSqlValue (SqlText) fvName
--           regDate <- fromSqlClock fvRegDate
--           -- unregDate <- fromSqlClockMaybe fvUnregDate
--           -- let key = fromJust $ fromSqlValue (SqlVarChar 1024) fvKey
--           -- return (Company unp name regDate unregDate key)
--           return (0::Int)

          
--------------------------------------------------------------------------------