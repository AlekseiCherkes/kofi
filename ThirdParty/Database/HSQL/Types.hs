-- #hide
module Database.HSQL.Types where

import Control.Concurrent.MVar
---------------------------------------
-- My fixing
--import Control.Exception 		-- old
import Control.OldException		-- my
---------------------------------------

import Data.Dynamic
import Foreign
import Foreign.C

type FieldDef = (String, SqlType, Bool)

data SqlType
	= SqlChar          Int               -- ODBC, MySQL, PostgreSQL
	| SqlVarChar       Int               -- ODBC, MySQL, PostgreSQL, MSI
	| SqlLongVarChar   Int               -- ODBC
	| SqlText                            --     ,      , PostgreSQL, MSI
	| SqlWChar         Int               -- ODBC
	| SqlWVarChar      Int               -- ODBC
	| SqlWLongVarChar  Int               -- ODBC
	| SqlDecimal       Int Int           -- ODBC
	| SqlNumeric       Int Int           -- ODBC, MySQL, PostgreSQL
	| SqlSmallInt                        -- ODBC, MySQL, PostgreSQL
	| SqlMedInt                          --     , MySQL,           
	| SqlInteger                         -- ODBC, MySQL, PostgreSQL, MSI
	| SqlReal                            -- ODBC, MySQL, PostgreSQL
	| SqlFloat                           -- ODBC
	| SqlDouble                          -- ODBC, MySQL, PostgreSQL
	| SqlBit                             -- ODBC,      , PostgreSQL
	| SqlTinyInt                         -- ODBC, MySQL, PostgreSQL
	| SqlBigInt                          -- ODBC, MySQL, PostgreSQL, MSI
	| SqlBinary        Int               -- ODBC,      , PostgreSQL
	| SqlVarBinary     Int               -- ODBC,      , PostgreSQL
	| SqlLongVarBinary Int               -- ODBC
	| SqlDate                            -- ODBC, MySQL, PostgreSQL
	| SqlTime                            -- ODBC, MySQL, PostgreSQL
	| SqlTimeTZ                          --     ,      , PostgreSQL
	| SqlAbsTime                         --     ,      , PostgreSQL
	| SqlRelTime                         --     ,      , PostgreSQL
	| SqlTimeInterval                    --     ,      , PostgreSQL
	| SqlAbsTimeInterval                 --     ,      , PostgreSQL
	| SqlTimeStamp                       -- ODBC, MySQL
	| SqlDateTime                        --     , MySQL
	| SqlDateTimeTZ                      --     , MySQL, PostgreSQL
	| SqlYear                            --     , MySQL
	| SqlSET                             --     , MySQL
	| SqlENUM                            --     , MySQL
	| SqlBLOB                            --     , MySQL,           , MSI
	| SqlMoney                           --     ,      , PostgreSQL
	| SqlINetAddr                        --     ,      , PostgreSQL
	| SqlCIDRAddr                        --     ,      , PostgreSQL
	| SqlMacAddr                         --     ,      , PostgreSQL
	| SqlPoint                           --     ,      , PostgreSQL
	| SqlLSeg                            --     ,      , PostgreSQL
	| SqlPath                            --     ,      , PostgreSQL
	| SqlBox                             --     ,      , PostgreSQL
	| SqlPolygon                         --     ,      , PostgreSQL
	| SqlLine                            --     ,      , PostgreSQL
	| SqlCircle                          --     ,      , PostgreSQL
	| SqlUnknown Int                     -- ^ HSQL returns @SqlUnknown tp@ for all
	                                     -- columns for which it cannot determine
	                                     -- the right type. The @tp@ here is the
	                                     -- internal type code returned from the
	                                     -- backend library
	deriving (Eq, Show)

data SqlError
   = SqlError
		{ seState       :: String
		, seNativeError :: Int
		, seErrorMsg    :: String
		}
   | SqlNoData
   | SqlInvalidHandle
   | SqlStillExecuting
   | SqlNeedData
   | SqlBadTypeCast
   		{ seFieldName :: String
   		, seFieldType :: SqlType
   		}
   | SqlFetchNull
   		{ seFieldName :: String
		}
   | SqlUnknownField
		{ seFieldName :: String
		}
   | SqlUnsupportedOperation
   | SqlClosedHandle
#ifdef __GLASGOW_HASKELL__
   deriving Typeable
#endif

sqlErrorTc :: TyCon
sqlErrorTc = mkTyCon "Database.HSQL.SqlError"

#ifndef __GLASGOW_HASKELL__
instance Typeable SqlError where
	typeOf _ = mkAppTy sqlErrorTc []
#endif

instance Show SqlError where
	showsPrec _ (SqlError{seErrorMsg=msg}) = showString msg
	showsPrec _ SqlNoData                  = showString "No data"
	showsPrec _ SqlInvalidHandle           = showString "Invalid handle"
	showsPrec _ SqlStillExecuting          = showString "Stlll executing"
	showsPrec _ SqlNeedData                = showString "Need data"
	showsPrec _ (SqlBadTypeCast name tp)   = showString ("The type of " ++ name ++ " field can't be converted to " ++ show tp ++ " type")
	showsPrec _ (SqlFetchNull name)        = showString ("The value of " ++ name ++ " field is null")
	showsPrec _ (SqlUnknownField name)     = showString ("Unknown field name: " ++ name)
	showsPrec _ SqlUnsupportedOperation    = showString "Unsupported operation"
	showsPrec _ SqlClosedHandle            = showString "The referenced handle is already closed"

-- | A 'Connection' type represents a connection to a database, through which you can operate on the it.
-- In order to create the connection you need to use the @connect@ function from the module for
-- your prefered backend.
data Connection
  =  Connection
		{ connDisconnect :: IO ()
		, connExecute :: String -> IO ()
		, connQuery :: String -> IO Statement
		, connTables :: IO [String]
		, connDescribe :: String -> IO [FieldDef]
		, connBeginTransaction :: IO ()
		, connCommitTransaction :: IO ()
		, connRollbackTransaction :: IO ()
		, connClosed :: MVar Bool
		}

-- | The 'Statement' type represents a result from the execution of given SQL query.
data Statement
  =  Statement
		{ stmtConn   :: Connection
		, stmtClose  :: IO ()
		, stmtFetch  :: IO Bool
		, stmtGetCol :: forall a . Int -> FieldDef -> (FieldDef -> CString -> Int -> IO a) -> IO a
		, stmtFields :: [FieldDef]
		, stmtClosed :: MVar Bool
		}


class SqlBind a where
	-- This allows for faster conversion for eq. integral numeric types, etc.
	-- Default version uses fromSqlValue.
	fromSqlCStringLen :: FieldDef -> CString -> Int -> IO a
	fromSqlCStringLen (name,sqlType,_) cstr cstrLen
	  | cstr == nullPtr = throwDyn (SqlFetchNull name)
	  | otherwise       = do 
	      str <- peekCStringLen (cstr, cstrLen)
	      case fromSqlValue sqlType str of
	        Nothing -> throwDyn (SqlBadTypeCast name sqlType)
	        Just v  -> return v

	fromSqlValue :: SqlType -> String -> Maybe a
	toSqlValue   :: a -> String
