module ClientConfig
       where

import Crypto

import System.Time

--------------------------------------------------------------------------------
-- Client config data types
--------------------------------------------------------------------------------

data CompanyConfig = ClientConfig { unp :: String
                                  , name :: String
                                  , registryDate :: CalendarTime
                                  , privateKey :: RSAKey }
                   deriving(Read, Show)
                     
data AccountConfig = AccountConfig { id :: String
                                   , cyrrencyCode :: Int }
                   deriving(Read, Show)  
  
--------------------------------------------------------------------------------
                                   