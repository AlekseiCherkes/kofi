module DataModel
    where 

data Status = DataStatus { id :: Int
                         , message :: String }
              deriving (Show)

data Company = Company { unp :: Int
                       , name :: String
                       , registryData :: Maybe CalendarTime
                       , unregistryData :: Maybe CalendarTime
                       , openKey :: Integer }
               deriving (Show)

data Account = Account { id :: Int
                       , ownerId :: Int
                       , ballance :: Int
                       , openData :: CalendarTime
                       , closeDate :: CalendarTime
                       , currencyCode :: Int }
               deriving (Show)                                                 
            