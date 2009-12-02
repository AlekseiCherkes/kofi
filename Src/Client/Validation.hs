
module Validation where


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


isValidUnp String -> Bool
isValidUnp lexem = (length lexem == 9) and isValidInteger lexem

isValidAcc String -> Bool
isValidAcc lexem = (length lexem == 13) and isValidInteger lexem