module Types 
       where

import Data.List

import Validation

type Name = String

data UNP  = C_UNP  { unp2str :: String} deriving (Read, Show)
data BIC  = C_BIC  { bic2str :: String} deriving (Read, Show)
data ACC  = C_ACC  { acc2str :: String} deriving (Read, Show)

instance Eq UNP where 
  (==) op1 op2 = isPrefixOf s1 s2 && isSuffixOf s1 s2
    where s1 = unp2str op1
          s2 = unp2str op2
          
instance Eq BIC where 
  (==) op1 op2 = isPrefixOf s1 s2 && isSuffixOf s1 s2
    where s1 = bic2str op1
          s2 = bic2str op2


instance Eq ACC where 
  (==) op1 op2 = isPrefixOf s1 s2 && isSuffixOf s1 s2
    where s1 = acc2str op1
          s2 = acc2str op2


str2bic :: String -> BIC
str2bic str = if (isValidBic str) then (C_BIC str) else error ("Invalid BIC: " ++ str) 

str2unp :: String -> UNP
str2unp str = if (isValidUnp str)  then (C_UNP str) else error ("Invalid UNP: " ++ str)

str2acc :: String -> ACC
str2acc str = if (isValidAcc str) then (C_ACC str) else error ("Invalid BIC: " ++ str)
                                        

data AccountPK = AccountPK { accId   :: ACC
                           , bankBic :: BIC }
                deriving (Read, Show)

type Base64               = String
type RSAKey               = (Base64, Base64)
type MessageBody          = String
type EncryptedMessageBody = Base64


