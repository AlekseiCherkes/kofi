module Message
    where

import System.Time

import Types

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

data SenderId = ClientId UNP  -- Client UNP
              | BankId   BIC    -- BIC (ignoring now)
              deriving (Read, Show)

data Message = Message { senderId :: SenderId
                       , digest   :: Base64
                       , text     :: String -- Serialized Request or Response
                       }
             deriving (Read, Show)
                      
--------------------------------------------------------------------------------
-- Requests
--------------------------------------------------------------------------------

data TransactionPriority = Urgent | Normal
                           deriving (Read, Show)

data AccountPK = AccountPK { accountId:: ACC
                           , banckBic :: BIC }
               deriving (Read, Show)   

data CommitedTransaction = CommitedTransaction { reason        :: String
                                               , creditAccount :: AccountPK
                                               , debitAccount  :: AccountPK
                                               , amount        :: Double
                                               , priority      :: TransactionPriority
                                               }
                         deriving (Read, Show)

data Request = CommitTransaction CommitedTransaction
             | GetBalance AccountPK
             | GetStatement AccountPK CalendarTime CalendarTime -- без ошибок
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
