module Types 
       where

import Validation

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Name = String
type Base64 = String
type RSAKey = (Base64, Base64)
type MessageBody = String
type EncryptedMessageBody = Base64

data UNP  = C_UNP  { unp2str :: String} deriving (Read, Show, Eq)
data BIC  = C_BIC  { bic2str :: String} deriving (Read, Show, Eq)
data ACC  = C_ACC  { acc2str :: String} deriving (Read, Show, Eq)

data AccountPK = AccountPK { accId   :: ACC
                           , bankBic :: BIC }
                deriving (Read, Show, Eq)

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

str2bic :: String -> BIC
str2bic str = if (isValidBic str) then (C_BIC str) else error ("Invalid BIC: " ++ str) 

str2unp :: String -> UNP
str2unp str = if (isValidUnp str)  then (C_UNP str) else error ("Invalid UNP: " ++ str)

str2acc :: String -> ACC
str2acc str = if (isValidAcc str) then (C_ACC str) else error ("Invalid BIC: " ++ str)
                                        
--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------
