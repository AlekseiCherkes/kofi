module DBInfo
       where

import Database.HaskellDB.DBSpec
import Database.HaskellDB.DBSpec.PPHelpers

--------------------------------------------------------------------------------
-- Database declaration
--------------------------------------------------------------------------------

serverDBInfo = DBInfo { dbname = "ServerDB"
                      , opts = DBOptions { useBString = False
                                         , makeIdent = mkIdentPreserving
                                         }
                      , tbls = [company, account, status]
                      }

company = TInfo {tname = "Company",
	         cols = [ CInfo {cname = "unp", descr = (IntegerT, False)}
                        , CInfo {cname = "name", descr = (StringT, False)}
                        , CInfo {cname = "registry_date", descr = (CalendarTimeT, False)}
                        , CInfo {cname = "unregistry_date", descr = (CalendarTimeT, True)}
                        , CInfo {cname = "open_key", descr = (IntegerT, False)}
                        ]
                }

account = TInfo {tname = "Account",
                 cols = [ CInfo {cname = "id", descr = (IntegerT, False)}
                        , CInfo {cname = "owner_id", descr = (IntegerT, False)}
                        , CInfo {cname = "ballance", descr = (DoubleT, False)}
                        , CInfo {cname = "open_date", descr = (CalendarTimeT, False)}
                        , CInfo {cname = "close_date", descr = (CalendarTimeT, True)}
                        ]
                }

transaction = TInfo {tname = "Transaction",
                     cols = [ CInfo {cname = "id", descr = (IntegerT, False)}
                            , CInfo {cname = "commit_date", descr = (CalendarTimeT, False)}
                            , CInfo {cname = "recive_date", descr = (CalendarTimeT, False)}
                            , CInfo {cname = "status_id", descr = (IntegerT, False)}
--                          , CInfo {cname = "content", descr = (StringT, False)}
                            , CInfo {cname = "reason", descr = (StringT, False)}
                            , CInfo {cname = "credit_account_id", descr = (IntegerT, False)}
                            , CInfo {cname = "debit_account_id", descr = (IntegerT, False)}
                            , CInfo {cname = "amount", descr = (DoubleT, False)}
                            , CInfo {cname = "priority", descr = (IntT, False)}
                            ]
                            }

status = TInfo {tname = "Status",
                  cols = [ CInfo {cname = "id", descr = (IntegerT, False)}
                         , CInfo {cname = "message", descr = (StringT, False)}
                         ]
               }

--------------------------------------------------------------------------------
