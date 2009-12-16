module Validation where
import Types


isValidInteger :: String -> Bool
isValidInteger lexem = 
    case (readsPrec 0 lexem)::[(Integer, String)] of
        []        -> False
        [(i,"")]  -> True
        otherwise -> False
        

isValidDouble :: String -> Bool
isValidDouble lexem = 
    case (readsPrec 0 lexem)::[(Double, String)] of
        []        -> False
        [(i,"")]  -> True
        otherwise -> False
        
        
isValidAmount :: String -> Bool
isValidAmount = isValidDouble

isValidBic :: String -> Bool
isValidBic str = (length str == 9) && isValidInteger str

isValidUnp :: String -> Bool
isValidUnp str = (length str == 13) && isValidInteger str

isValidAcc :: String -> Bool
isValidAcc str = (length str == 13) && isValidInteger str



str2bic :: String -> BIC
str2bic str = if (isValidBic str) then str else error "Invalid BIC."

str2unp :: String -> UNP
str2unp str = if (isValidUnp str)  then str else error "Invalid UNP."

str2acc :: String -> ACC
str2acc str = if (isValidAcc str) then str else error "Invalid BIC."


