#include "Rts.h"
#include "Trace.h"
#include "rts/Threads.h"
#include "sm/Storage.h"
#include <stdio.h>

//casts
#define TO_NOREC(x) ((StgTVar *)x)
#define TO_TL2(x)   ((StgTL2TVar *)x)

#define PASTM_SUCCESS              ((StgClosure*)(void*)&stg_PA_STM_SUCCESS_closure)
#define PASTM_FAIL                 ((StgClosure*)(void*)&stg_PA_STM_FAIL_closure)
#define NO_PTREC                   ((StgPTRecHeader *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER               &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER            &stg_PTREC_WITHOUTK_info
#define WRITESET_HEADER            &stg_WRITE_SET_info


void writeTL2TVarIO(StgTL2TVar * tvar, StgClosure * new, Capability * cap){
    tvar->current_value = new;
    dirty_TL2_TVAR(cap, tvar);
}

void writeNoRecTVarIO(StgTVar * tvar, StgClosure * new, Capability * cap){
    tvar->current_value = new;
    dirty_TVAR(cap, tvar);
}
