#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
void DatabaseziSQLiteziBase_d3UD(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsPtr a2, HsInt32 a3, HsPtr a4)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkPtr(cap,a2)),rts_mkInt32(cap,a3)),rts_mkPtr(cap,a4))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3UD",cap);
rts_unlock(cap);
}
 
void DatabaseziSQLiteziBase_d3V0(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsPtr a2, HsInt32 a3, HsPtr a4)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkPtr(cap,a2)),rts_mkInt32(cap,a3)),rts_mkPtr(cap,a4))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3V0",cap);
rts_unlock(cap);
}
 
HsInt32 DatabaseziSQLiteziBase_d3V9(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsInt32 a2, HsPtr a3, HsInt32 a4, HsPtr a5)
{
Capability *cap;
HaskellObj ret;
HsInt32 cret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkInt32(cap,a2)),rts_mkPtr(cap,a3)),rts_mkInt32(cap,a4)),rts_mkPtr(cap,a5))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3V9",cap);
cret=rts_getInt32(ret);
rts_unlock(cap);
return cret;
}
 
void DatabaseziSQLiteziBase_d3Vi(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsInt32 a2, HsPtr a3)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkInt32(cap,a2)),rts_mkPtr(cap,a3))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3Vi",cap);
rts_unlock(cap);
}
 
void DatabaseziSQLiteziBase_d3Vr(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3Vr",cap);
rts_unlock(cap);
}
 
HsWord32 DatabaseziSQLiteziBase_d3VA(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
Capability *cap;
HaskellObj ret;
HsWord32 cret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3VA",cap);
cret=rts_getWord32(ret);
rts_unlock(cap);
return cret;
}
 
void DatabaseziSQLiteziBase_d3VJ(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsInt32 a2, HsPtr a3, HsPtr a4, HsInt64 a5)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkInt32(cap,a2)),rts_mkPtr(cap,a3)),rts_mkPtr(cap,a4)),rts_mkInt64(cap,a5))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3VJ",cap);
rts_unlock(cap);
}
 
void DatabaseziSQLiteziBase_d3VS(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3VS",cap);
rts_unlock(cap);
}
 
HsWord32 DatabaseziSQLiteziBase_d3W1(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1, HsInt32 a2, HsPtr a3, HsPtr a4)
{
Capability *cap;
HaskellObj ret;
HsWord32 cret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(cap,a1)),rts_mkInt32(cap,a2)),rts_mkPtr(cap,a3)),rts_mkPtr(cap,a4))) ,&ret);
rts_checkSchedStatus("DatabaseziSQLiteziBase_d3W1",cap);
cret=rts_getWord32(ret);
rts_unlock(cap);
return cret;
}
#ifdef __cplusplus
}
#endif

