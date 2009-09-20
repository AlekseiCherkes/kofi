import KofiDB
import System.IO
import Database.HaskellDB
import Database.HaskellDB.HSQL.SQLite3
import Database.HaskellDB.DBSpec.DBSpecToDatabase

opts = SQLiteOptions {
         filepath = "kofi.db",
         mode = ReadWriteMode
       }

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect opts

createDB :: Database -> IO ()
createDB db = dbSpecToDatabase db kofiDBInfo

main :: IO ()
main = withDB createDB