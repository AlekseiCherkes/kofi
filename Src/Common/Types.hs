module Types 
       where
       
type Name = String
type UNP  = String
type BIC  = String
type ACC  = String



--data UNP  = C_UNP  { unp2str :: String} -- do not derive (Read, Show) beter implement it here
--data BIC  = C_BIC  { bic2str :: String}
--data ACC  = C_ACC  { acc2str :: String}

--instance Show UNP where show (C_UNP str) = str
--instance Show BIC where show (C_BIC str) = str
--instance Show ACC where show (C_ACC str) = str

-- Types.hs and Validation.hs are to be united
--instance Read UNP where readsPrec _ line = tryParse isValidUnp C_UNP (take 1 $ words line) line                     
--instance Read BIC where readsPrec _ line = tryParse isValidBic C_BIC (take 1 $ words line) line                
--instance Read ACC where readsPrec _ line = tryParse isValidAcc C_ACC (take 1 $ words line) line
                        
                                                 
--tryParse :: (String -> Bool) -> (String -> a) -> [String] -> String -> [(a,String)]
--tryParse _       _           []        _    = []
--tryParse isValid constructor [s]       line = if (isValid s) then [(constructor s, drop (length s) line)] else []
--tryParse _       _           (_:(_:_)) _    = [] -- impossible, though GHC has no idea I have used "take 1"
                                        

data AccountPK = AccountPK { accId   :: ACC
                           , bankBic :: BIC }
                deriving (Read, Show)

type Base64               = String
type RSAKey               = (Base64, Base64)
type MessageBody          = String
type EncryptedMessageBody = Base64


