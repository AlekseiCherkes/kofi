import KofiDB
import System.IO
import Database.HaskellDB
import Database.HaskellDB.HSQL.SQLite3
import Database.HaskellDB.DBSpec.DBSpecToDatabase

opts = SQLiteOptions {
         filepath = "kofi.db",
         mode = WriteMode
       }

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect opts

--insPerson :: Database -> IO ()
--insPerson db = 
--    insert db persons
--               (name  << first_name "Gerrit")

--fillDB :: Database -> IO ()
--fillDB db = insPerson db

main :: IO ()
--main = withDB fillDB
main = print "Sorry, not implemented yet."