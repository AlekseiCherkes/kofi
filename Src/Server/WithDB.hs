module WithDB(withDB)
    where 

import System.IO

dataBaseFilePath = "server.db"

-- withDB :: (Database -> IO a) -> IO a
-- withDB = sqliteConnect opts
withDB = print "asd"