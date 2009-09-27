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
               tbls = [persons]
             }

persons = TInfo {tname = "persons",
	    cols = [ CInfo {cname = "first_name", descr = (StringT, True )}
                   , CInfo {cname = "second_name", descr = (StringT, True )}
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