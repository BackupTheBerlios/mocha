#define IN_STG_CODE 0
#include "RtsAPI.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
void MochaziExportHelper_d1J8d(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
SchedulerStatus rc;
HaskellObj ret;
rts_lock();
rc=rts_evalIO(rts_apply((HaskellObj)runIO_closure,rts_apply((StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(a1))) ,&ret);
rts_checkSchedStatus("MochaziExportHelper_d1J8d",rc);
rts_unlock();
}
 
HsBool MochaziExportHelper_d1J8e(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
SchedulerStatus rc;
HaskellObj ret;
HsBool cret;
rts_lock();
rc=rts_evalIO(rts_apply((HaskellObj)runIO_closure,rts_apply((StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(a1))) ,&ret);
rts_checkSchedStatus("MochaziExportHelper_d1J8e",rc);
cret=rts_getBool(ret);
rts_unlock();
return cret;
}
 
HsPtr MochaziExportHelper_d1J8f(StgStablePtr the_stableptr, void* original_return_addr, HsPtr a1)
{
SchedulerStatus rc;
HaskellObj ret;
HsPtr cret;
rts_lock();
rc=rts_evalIO(rts_apply((HaskellObj)runIO_closure,rts_apply((StgClosure*)deRefStablePtr(the_stableptr),rts_mkPtr(a1))) ,&ret);
rts_checkSchedStatus("MochaziExportHelper_d1J8f",rc);
cret=rts_getPtr(ret);
rts_unlock();
return cret;
}
 
void MochaziExportHelper_d1J8g(StgStablePtr the_stableptr, void* original_return_addr)
{
SchedulerStatus rc;
HaskellObj ret;
rts_lock();
rc=rts_evalIO(rts_apply((HaskellObj)runIO_closure,(StgClosure*)deRefStablePtr(the_stableptr)) ,&ret);
rts_checkSchedStatus("MochaziExportHelper_d1J8g",rc);
rts_unlock();
}
#ifdef __cplusplus
}
#endif

