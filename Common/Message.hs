module Message
    where

--------------------------------------------------------------------------------

import System.Time

--------------------------------------------------------------------------------

data TransactionPriority = Urgent | Normal
                           deriving (Read, Show)

data CommitedTransaction = CommitedTransaction { reason :: String
                                               , creditAccountId :: Integer
                                               , debitAccountId :: Integer
                                               , amount :: Double
                                               , priority :: TransactionPriority
                                       }
                   deriving (Read, Show)

--------------------------------------------------------------------------------

data Message = Message { unp :: Integer
                       , body :: String
                       , digest :: Integer -- от body
                       }
               deriving (Read, Show)

data Request = CommitTransaction CommitedTransaction
             | GetBalance
             | GetExtract CalendarTime CalendarTime -- без ошибок
             | GetLog CalendarTime CalendarTime     -- с ошибками
               deriving (Read, Show)

data Response = Balance Double
              | Log [String]
              | Extract [String]
                deriving (Read, Show)

--------------------------------------------------------------------------------
isUrgent :: TransactionPriority -> Bool
isUrgent Normal = False
isUrgent Urgent = True