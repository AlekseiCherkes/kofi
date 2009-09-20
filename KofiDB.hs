module KofiDB
    where

import System.IO
import Database.HaskellDB.DBLayout
import Database.HaskellDB

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