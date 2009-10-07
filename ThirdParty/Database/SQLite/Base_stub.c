#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
void DatabaseziSQLiteziBase_d4A2(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsPtr a2, HsInt32 a3, HsPtr a4)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkPtr(cap,a2)),rts_mkInt32(cap,a3)),rts_mkPtr(cap,a4))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4A2",cap);
rts_unlock(cap);
}
 
void DatabaseziSQLiteziBase_d4Ap(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsPtr a2, HsInt32 a3, HsPtr a4)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkPtr(cap,a2)),rts_mkInt32(cap,a3)),rts_mkPtr(cap,a4))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4Ap",cap);
rts_unlock(cap);
}
 
HsInt32 DatabaseziSQLiteziBase_d4Ay(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsInt32 a2, HsPtr a3, HsInt32 a4, HsPtr a5)
{
Capability *cap;
HaskellObj ret;
HsInt32 cret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkInt32(cap,a2)),rts_mkPtr(cap,a3)),rts_mkInt32(cap,a4)),rts_mkPtr(cap,a5))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4Ay",cap);
cret=rts_getInt32(ret);
rts_unlock(cap);
return cret;
}
 
void DatabaseziSQLiteziBase_d4AH(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsInt32 a2, HsPtr a3)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkInt32(cap,a2)),rts_mkPtr(cap,a3))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4AH",cap);
rts_unlock(cap);
}
 
void DatabaseziSQLiteziBase_d4AQ(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4AQ",cap);
rts_unlock(cap);
}
 
HsWord32 DatabaseziSQLiteziBase_d4AZ(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
Capability *cap;
HaskellObj ret;
HsWord32 cret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4AZ",cap);
cret=rts_getWord32(ret);
rts_unlock(cap);
return cret;
}
 
void DatabaseziSQLiteziBase_d4B8(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsInt32 a2, HsPtr a3, HsPtr a4, HsInt64 a5)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkInt32(cap,a2)),rts_mkPtr(cap,a3)),rts_mkPtr(cap,a4)),rts_mkInt64(cap,a5))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4B8",cap);
rts_unlock(cap);
}
 
void DatabaseziSQLiteziBase_d4Bh(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4Bh",cap);
rts_unlock(cap);
}
 
HsWord32 DatabaseziSQLiteziBase_d4Bq(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsInt32 a2, HsPtr a3, HsPtr a4)
{
Capability *cap;
HaskellObj ret;
HsWord32 cret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkInt32(cap,a2)),rts_mkPtr(cap,a3)),rts_mkPtr(cap,a4))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d4Bq",cap);
cret=rts_getWord32(ret);
rts_unlock(cap);
return cret;
}
#ifdef __cplusplus
}
#endif

