module Types where

type Base64 = String
type Name  = String
type UNP   = String
type BIC  = String
type ACC  = String 


data AccountPK = AccountPK { accId   :: ACC
                           , bankBic :: BIC }
                deriving (Read, Show) 

