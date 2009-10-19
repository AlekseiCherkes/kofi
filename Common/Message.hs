module Message
    where

import Entity
import System.Time

data Message = Message {unp :: Integer
                       , body :: String
                       , digest :: Integer -- от body
                       }
               deriving (Read, Show)

data Request = CommitTransaction Transaction
             | GetBalance
             | GetExtract CalendarTime CalendarTime -- без ошибок
             | GetLog CalendarTime CalendarTime     -- с ошибками
               deriving (Read, Show)

data Response = Balance Double
              | Log [String]
              | Extract [String]
                deriving (Read, Show)