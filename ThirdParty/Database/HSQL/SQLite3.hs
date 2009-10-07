{-# INCLUDE <fcntl.h> #-}
{-# INCLUDE <sqlite3.h> #-}
{-# LINE 1 "SQLite3.hsc" #-}
-----------------------------------------------------------------------------------------
{-# LINE 2 "SQLite3.hsc" #-}
{-| Module      :  Database.HSQL.SQLite3
    Copyright   :  (c) Krasimir Angelov 2005
    License     :  BSD-style

    Maintainer  :  kr.angelov@gmail.com
    Stability   :  provisional
    Portability :  portable

    The module provides interface to SQLite3
-}
-----------------------------------------------------------------------------------------

module Database.HSQL.SQLite3(connect, module Database.HSQL) where

import Database.HSQL
import Database.HSQL.Types
import Foreign
import Foreign.C
import System.IO
import Control.Monad(when)
import Control.Exception(throwDyn)
import Control.Concurrent.MVar


{-# LINE 26 "SQLite3.hsc" #-}

{-# LINE 27 "SQLite3.hsc" #-}

type SQLite3 = Ptr ()

foreign import ccall sqlite3_open :: CString -> (Ptr SQLite3) -> IO Int
foreign import ccall sqlite3_errmsg :: SQLite3 -> IO CString
foreign import ccall sqlite3_close :: SQLite3 -> IO ()
foreign import ccall sqlite3_exec :: SQLite3 -> CString -> FunPtr () -> Ptr () -> Ptr CString -> IO CInt
foreign import ccall sqlite3_get_table ::   SQLite3 -> CString -> Ptr (Ptr CString) -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO CInt
foreign import ccall sqlite3_free_table :: Ptr CString -> IO ()
foreign import ccall sqlite3_free :: CString -> IO ()

foreign import ccall "strlen" strlen :: CString -> IO CInt

-----------------------------------------------------------------------------------------
-- routines for handling exceptions
-----------------------------------------------------------------------------------------

handleSqlResult :: CInt -> Ptr CString -> IO ()
handleSqlResult res ppMsg
	| res == (0) = return ()
{-# LINE 47 "SQLite3.hsc" #-}
	| otherwise = do
		pMsg <- peek ppMsg
		msg <- peekCString pMsg
		sqlite3_free pMsg
		throwDyn (SqlError "E" (fromIntegral res) msg)

-----------------------------------------------------------------------------------------
-- Connect
-----------------------------------------------------------------------------------------

connect :: FilePath -> IOMode -> IO Connection
connect fpath mode =
	alloca $ \psqlite ->
	  withCString fpath $ \pFPath -> do
		res <- sqlite3_open pFPath psqlite
		sqlite <- peek psqlite
		when (res /= (0)) $ do
{-# LINE 64 "SQLite3.hsc" #-}
			pMsg <- sqlite3_errmsg sqlite
			msg <- peekCString pMsg
			throwDyn (SqlError
			            { seState = "C"
			            , seNativeError = 0
			            , seErrorMsg = msg
			            })
		refFalse <- newMVar False
		let connection = Connection
			{ connDisconnect = sqlite3_close sqlite
			, connClosed     = refFalse
			, connExecute    = execute sqlite
			, connQuery      = query connection sqlite
			, connTables     = tables connection sqlite
			, connDescribe   = describe connection sqlite
			, connBeginTransaction = execute sqlite "BEGIN TRANSACTION"
			, connCommitTransaction = execute sqlite "COMMIT TRANSACTION"
			, connRollbackTransaction = execute sqlite "ROLLBACK TRANSACTION"
			}
		return connection
	where
		oflags1 = case mode of
	    	  ReadMode      -> (0)
{-# LINE 87 "SQLite3.hsc" #-}
	    	  WriteMode     -> (1)
{-# LINE 88 "SQLite3.hsc" #-}
	    	  ReadWriteMode -> (2)
{-# LINE 89 "SQLite3.hsc" #-}
	    	  AppendMode    -> (1024)
{-# LINE 90 "SQLite3.hsc" #-}

		execute :: SQLite3 -> String -> IO ()
		execute sqlite query =
			withCString query $ \pQuery -> do
			alloca $ \ppMsg -> do
				res <- sqlite3_exec sqlite pQuery nullFunPtr nullPtr ppMsg
				handleSqlResult res ppMsg

		query :: Connection -> SQLite3 -> String -> IO Statement
		query connection sqlite query = do
			withCString query $ \pQuery -> do
			alloca $ \ppResult -> do
			alloca $ \pnRow -> do
			alloca $ \pnColumn -> do
			alloca $ \ppMsg -> do
				res <- sqlite3_get_table sqlite pQuery ppResult pnRow pnColumn ppMsg
				handleSqlResult res ppMsg
				pResult <- peek ppResult
				rows    <- fmap fromIntegral (peek pnRow)
				columns <- fmap fromIntegral (peek pnColumn)
				defs <- getFieldDefs pResult 0 columns
				refFalse <- newMVar False
				refIndex <- newMVar 0
				return (Statement
				              { stmtConn   = connection
				              , stmtClose  = sqlite3_free_table pResult
				              , stmtFetch  = fetch refIndex rows
				              , stmtGetCol = getColValue pResult refIndex columns rows
				              , stmtFields = defs
				              , stmtClosed = refFalse
				              })
			where
				getFieldDefs :: Ptr CString -> Int -> Int -> IO [FieldDef]
				getFieldDefs pResult index count
					| index >= count = return []
					| otherwise         = do
						name <- peekElemOff pResult index >>= peekCString
						defs <- getFieldDefs pResult (index+1) count
						return ((name,SqlText,True):defs)

		tables :: Connection -> SQLite3 -> IO [String]
		tables connection sqlite = do
			stmt <- query connection sqlite "select tbl_name from sqlite_master"
			collectRows (\stmt -> getFieldValue stmt "tbl_name") stmt

		describe :: Connection -> SQLite3 -> String -> IO [FieldDef]
		describe connection sqlite table = do
			stmt <- query connection sqlite ("pragma table_info("++table++")")
			collectRows getRow stmt
			where
				getRow stmt = do
					name <- getFieldValue stmt "name"
					notnull <- getFieldValue stmt "notnull"
					return (name, SqlText, notnull=="0")

		fetch tupleIndex countTuples =
			modifyMVar tupleIndex (\index -> return (index+1,index < countTuples))

		getColValue pResult refIndex columns rows colNumber fieldDef f = do
			index <- readMVar refIndex
			when (index > rows) (throwDyn SqlNoData)
			pStr <- peekElemOff pResult (columns*index+colNumber)
			if pStr == nullPtr
			  then f fieldDef pStr 0
			  else do strLen <- strlen pStr
				  f fieldDef pStr (fromIntegral strLen)
