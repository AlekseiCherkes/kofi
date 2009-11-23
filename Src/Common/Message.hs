module Message
    where

import System.Time

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

type Base64 = String

data SenderId = ClientId String  -- Client UNP
              | BankId String    -- BIC (ignoring now)
              deriving (Read, Show)

data Message = Message { senderId :: SenderId
                       , digest :: Base64
                       , text :: String -- Serialized Request or Response
                       }
             deriving (Read, Show)
                      
--------------------------------------------------------------------------------
-- Requests
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

data Request = CommitTransaction CommitedTransaction
             | GetBalance
             | GetExtract CalendarTime CalendarTime -- без ошибок
             | GetLog CalendarTime CalendarTime     -- с ошибками
             deriving (Read, Show)
                      
--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

data Response = Balance Double
              | Log [String]
              | Extract [String]
              deriving (Read, Show)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

isUrgent :: TransactionPriority -> Bool
isUrgent Normal = False
isUrgent Urgent = True

--------------------------------------------------------------------------------
