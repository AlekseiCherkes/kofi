module Queries
    where

import Entity

import Data.List
import Data.Maybe
import System.Time
import Database.HSQL -- .Types

--------------------------------------------------------------------------------
-- Database functions
--------------------------------------------------------------------------------

insertCompany company connection = do
  print $ "Inserting: " ++ (show company)
  print $ "Query: " ++ (show q)
  execute connection q
  where q = "INSERT INTO Company VALUES (" ++ values ++");"
        values = foldl (++) "" $ intersperse ", " [toSqlValue $ unp company, 
                                                   toSqlValue $ name company, 
                                                   toSqlValue $ toClockTime (registryDate company), 
                                                   clockValue $ unregistryDate company,
                                                   toSqlValue $ openKey company]
          where clockValue (Just a) = toSqlValue $ toClockTime a
                clockValue Nothing = "NULL"
                
  -- execute connection 

--------------------------------------------------------------------------------