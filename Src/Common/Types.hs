module Types 
       where

type Name = String
type UNP = String
type BIC = String
type ACC = String

data AccountPK = AccountPK { accId   :: ACC
                           , bankBic :: BIC }
                deriving (Read, Show)

type Base64 = String
type RSAKey = (Base64, Base64)
type MessageBody = String
type EncryptedMessageBody = Base64

