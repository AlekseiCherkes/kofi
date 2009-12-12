
module Validation where

type Name = String
type UNP  = String
type BIC  = String
type ACC  = String 


isValidInteger :: String -> Bool
isValidInteger lexem = 
    case (readsPrec 0 lexem)::[(Integer, String)] of
        []        -> False
        [(i,"")]  -> True
        otherwise -> False
        
isValidFloat :: String -> Bool
isValidFloat lexem = 
    case (readsPrec 0 lexem)::[(Foat, String)] of
        []        -> False
        [(i,"")]  -> True
        otherwise -> False
        
        
isValidAmount :: String -> Bool
isValidAmount = isValidFloat




isValidBic String -> Bool
isValidBic str = (length str == 3) && isValidInteger str

str2bic :: String -> BIC
str2bic str = if (isValidBic str) then str




isValidUnp String -> Bool
isValidUnp str = (length str == 9) && isValidInteger str

str2unp String -> UNP
srr2unp str = if (isValidUnp str)  then str




isValidAcc String -> Bool
isValidAcc str = (length str == 13) && isValidInteger str

str2acc String -> ACC
str2acc str = if (isValidAcc str) then str
