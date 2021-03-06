module Validation where

isValidInteger :: String -> Bool
isValidInteger lexem = 
    case (readsPrec 0 lexem)::[(Integer, String)] of
        []        -> False
        [(_,"")]  -> True
        otherwise -> False
        

isValidDouble :: String -> Bool
isValidDouble lexem = 
    case (readsPrec 0 lexem)::[(Double, String)] of
        []        -> False
        [(_,"")]  -> True
        otherwise -> False
        
        
isValidAmount :: String -> Bool
isValidAmount str = (isValidDouble str) && ((read str)::Double) > 0



isValidBic :: String -> Bool
isValidBic str = ((length str) `elem` [9]) && isValidInteger str

isValidUnp :: String -> Bool
isValidUnp str = ((length str) `elem` [13]) && isValidInteger str

isValidAcc :: String -> Bool
isValidAcc str = (length str == 13) && isValidInteger str

