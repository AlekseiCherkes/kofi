module Message
    where

--------------------------------------------------------------------------------

import System.Time

--------------------------------------------------------------------------------

type CipheredMessage = [String]

data UserId = ClientId Integer     -- UNP клиента
            | BankId Integer       -- в текущей реализации всегда 0
            deriving (Read, Show)

data Message = Message { userId :: UserId  -- 0 для сервера, UNP для клриента
                       , digest :: Integer -- от body
                       , text :: MessageText
                       }
               deriving (Read, Show)
                        
makeMessage :: Integer -> MessageText -> digestKey -> encryptionKey
makeMessage unp
                        
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

data Response = Balance Double
              | Log [String]
              | Extract [String]
                deriving (Read, Show)

--------------------------------------------------------------------------------

isUrgent :: TransactionPriority -> Bool
isUrgent Normal = False
isUrgent Urgent = True

--------------------------------------------------------------------------------