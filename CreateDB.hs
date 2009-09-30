import System.IO
import Database.HaskellDB
import Database.HaskellDB.DBLayout
import Database.HaskellDB.HSQL.SQLite3
import Database.HaskellDB.DBSpec.DBSpecToDatabase
import Database.HaskellDB.DBSpec.DBSpecToDBDirect

--------------------------------------------------------------------------------
-- Database information
--------------------------------------------------------------------------------

kofiDBInfo = DBInfo {
               dbname = "kofi", 
               opts = DBOptions{useBString = False}, 
               tbls = [companies, accounts, statuses]
             }

companies = TInfo {tname = "companies",
	           cols = [ CInfo {cname = "unp", descr = (IntegerT, False)}
                          , CInfo {cname = "name", descr = (StringT, False)}
                          , CInfo {cname = "registry_date", descr = (StringT, False)}
                          , CInfo {cname = "unregistry_date", descr = (StringT, True)}
                          , CInfo {cname = "open_key", descr = (IntegerT, False)}
                          ]
                  }

accounts = TInfo {tname = "accounts",
                  cols = [ CInfo {cname = "id", descr = (IntegerT, False)}
                         , CInfo {cname = "owner_id", descr = (IntegerT, False)}
                         , CInfo {cname = "ballance", descr = (IntegerT, False)}
                         , CInfo {cname = "open_date", descr = (CalendarTimeT, False)}
                         , CInfo {cname = "close_date", descr = (CalendarTimeT, True)}
                         , CInfo {cname = "currency_code", descr = (IntT, False)}
                         ]
                 }

statuses = TInfo {tname = "statuses",
                  cols = [ CInfo {cname = "id", descr = (IntT, False)}
                         , CInfo {cname = "message", descr = (StringT, False)}
                         ]
                 }

  
--------------------------------------------------------------------------------
-- Create physical Database file and declaration modules.
--------------------------------------------------------------------------------

my_opts = SQLiteOptions {
         filepath = "kofi.db",
         mode = WriteMode
       }

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect my_opts

main :: IO ()
main = do 
  dbInfoToModuleFiles "./" "KofiDB" kofiDBInfo
  withDB (\ db -> dbSpecToDatabase db kofiDBInfo)

--------------------------------------------------------------------------------