module DataModel
    where 

data Status = Status { id :: Integer
                     , message :: String }
              deriving (Show)

data Company = Company { unp :: Integer
                       , name :: String
                       , registryData :: CalendarTime
                       , unregistryData :: Maybe CalendarTime
                       , openKey :: Integer }
               deriving (Show)

data Account = Account { id :: Integer
                       , ownerId :: Integer
                       , ballance :: Int
                       , openData :: CalendarTime
                       , closeDate :: Maybe CalendarTime
                     --, currencyCode :: Int }
               deriving (Show)

data TransactioinPriority = FixedDate | Normal;

data Transaction = Transaction { id :: Integer
                               , commitDate :: CalendarTime
                               , reciveData :: CalendarTime
                               , statusId :: Integer
                             --, content :: String
                               , reason :: String
                               , creditAccountId :: Integer
                               , debitAccountId :: Integer
                               , amount :: TransactionPriority
                               , priority :: }
                   deriving (Show)


                                           


            