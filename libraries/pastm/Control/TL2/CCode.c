#include "Rts.h"
#include "Trace.h"
#include "rts/Threads.h"
#include "sm/Storage.h"
#include <stdio.h>

#define TRUE 1
#define FALSE 0
#define KBOUND 20
#define START_FREQ 4

//casts
#define TO_WITHK(x) ((StgPTRecWithK*)x)
#define TO_WITHOUTK(x) ((StgPTRecWithoutK*)x)
#define TO_WRITE_SET(x) ((StgWriteSet*)x)
#define TO_CLOSURE(x) ((StgClosure*)x)
#define TO_OR_ELSE(x) ((StgPTRecOrElse *)x)
#define TO_NOREC(x) ((StgTVar *)x)
#define TO_TL2(x)   ((StgTL2TVar *)x)

#define PASTM_SUCCESS              ((StgClosure*)(void*)&stg_PA_STM_SUCCESS_closure)
#define PASTM_FAIL                 ((StgClosure*)(void*)&stg_PA_STM_FAIL_closure)
#define NO_PTREC                   ((StgPTRecHeader *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER               &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER            &stg_PTREC_WITHOUTK_info
#define WRITESET_HEADER            &stg_WRITE_SET_info

static volatile unsigned long version_clock = 0;

#define STATS

#ifdef STATS
static StgPASTMStats stats = {0, 0, 0, 0, 0};
#endif

StgPTRecHeader * pa_stmStartTransaction(Capability *cap, StgPTRecHeader * ptrec) {
    if(ptrec == NO_PTREC){
	ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
	ptrec->tail = TO_WITHOUTK(NO_PTREC);
    }
    
    ptrec->read_set = TO_WITHOUTK(NO_PTREC);
    ptrec->lastK = TO_WITHK(NO_PTREC);
    ptrec->write_set = TO_WRITE_SET(NO_PTREC);
    ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);
    
    ptrec->read_version = atomic_inc(&version_clock, 1);

    ptrec->capture_freq = ((unsigned long)START_FREQ << 32) + START_FREQ ; 
    ptrec->numK = 0;

    return ptrec;
}

static void clearTRec(StgPTRecHeader * trec){
    trec->read_set = TO_WITHOUTK(NO_PTREC);
    trec->lastK = TO_WITHK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->retry_stack = TO_OR_ELSE(NO_PTREC);
}

static StgPTRecWithK * validate(StgPTRecHeader * trec){
    unsigned long stamp = trec->read_version;
    StgPTRecWithoutK * ptr = trec->read_set;
    StgPTRecWithK * checkpoint = TO_WITHK(PASTM_SUCCESS);
    StgInt kCount = 0;
    
    while(ptr != TO_WITHOUTK(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ptr->tvar);
	if((tvar->lock && tvar->lock != stamp) || tvar->stamp > stamp){
	    clearTRec(trec);
	    return TO_WITHK(PASTM_FAIL);
	}
	ptr = ptr->next;
    }
    
    return TO_WITHK(PASTM_SUCCESS);
}


StgClosure * pa_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
			    StgTL2TVar * tvar, StgClosure * k){
    StgWriteSet * ws = trec->write_set;

    while(ws != TO_WRITE_SET(NO_PTREC)){
        if(TO_TL2(ws->tvar) == tvar){
            return ws->val;
        }
        ws = ws->next;
    }
    
    StgClosure * val; 
    unsigned long stamp1, stamp2;
    

    do{
	load_load_barrier();
	stamp1 = tvar->stamp;
	load_load_barrier();
	val = tvar->current_value;
	load_load_barrier();
	stamp2 = tvar->stamp;
	load_load_barrier();
    }while(tvar->lock);

    if(stamp2 > trec->read_version || stamp1 != stamp2){
	trec->read_version = atomic_inc(&version_clock, 1);
	clearTRec(trec);
	return PASTM_FAIL;
    }
    
    StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
    SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
    entry->tvar = TO_NOREC(tvar);
    entry->read_value = val;
    entry->next = trec->read_set;
    trec->read_set = entry;
 
    return val; 
}

void pa_stmWriteTVar(Capability *cap,
		     StgPTRecHeader *trec,
		     StgTVar *tvar,
		     StgClosure *new_value) {
    StgWriteSet * newEntry = (StgWriteSet *) allocate(cap, sizeofW(StgWriteSet));
    SET_HDR(newEntry , &stg_WRITE_SET_info, CCS_SYSTEM);
    newEntry->tvar = tvar;
    newEntry->val = new_value;
    newEntry->next = trec->write_set;
    trec->write_set = newEntry;
}

void releaseLocks(StgWriteSet * ws, StgWriteSet * sentinel){
    while(ws != sentinel){
	StgTL2TVar * tvar = TO_TL2(ws->tvar);
	tvar->lock = 0;
	ws = ws->next;
    }
}

StgPTRecWithK * pa_stmCommitTransaction(Capability *cap, StgPTRecHeader *trec) {
    unsigned long stamp = trec->read_version;
    //Acquire locks
    StgWriteSet * ptr = trec->write_set;
    StgWriteSet ** trailer = &(trec->write_set);
    while(ptr != TO_WRITE_SET(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ptr->tvar);
	unsigned long old = cas((StgVolatilePtr)&(tvar->lock), 0, stamp);
	if(old != 0){
	    if(old == stamp){//drop this from the write set, we've already seen it and the other one is newer
		*trailer = ptr->next;
		ptr = ptr->next;
		continue;
	    }else{//someone else locked this
		//validate read set
		releaseLocks(trec->write_set, ptr);
		clearTRec(trec);
		return TO_WITHK(PASTM_FAIL);
	    }
	}
	trailer = &(ptr->next);
	ptr = ptr->next;
    }
    
    unsigned long write_version = atomic_inc(&version_clock, 1);

    //validate read set
    ptr = trec->write_set;
    StgPTRecWithK * res = validate(trec);
    if(res != TO_WITHK(PASTM_SUCCESS)){
	releaseLocks(ptr, TO_WRITE_SET(NO_PTREC));
	trec->read_version = write_version;
	return res;
    }
    
    //push write set into global store
    ptr = trec->write_set;
    while(ptr != TO_WRITE_SET(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ptr->tvar);
	tvar->current_value = ptr->val;
	tvar->stamp = write_version;
	tvar->lock = 0;
	dirty_TL2_TVAR(cap, tvar);
	ptr = ptr->next;
    }

#ifdef STATS
    atomic_inc((StgVolatilePtr)&(stats.commitTimeFullAborts), cap->pastmStats.commitTimeFullAborts);
    atomic_inc((StgVolatilePtr)&(stats.eagerFullAborts), cap->pastmStats.eagerFullAborts);
    atomic_inc((StgVolatilePtr)&(stats.numCommits), 1);
    cap->pastmStats.commitTimeFullAborts = 0;
    cap->pastmStats.eagerFullAborts = 0;
#endif

    return TO_WITHK(PASTM_SUCCESS);
}

void pa_printSTMStats(){
#ifdef STATS
    printf("Commit Full Aborts = %lu\n", stats.commitTimeFullAborts);
    printf("Eager Full Aborts = %lu\n", stats.eagerFullAborts);
    printf("Total Aborts = %lu\n", stats.commitTimeFullAborts + stats.commitTimePartialAborts + 
	   stats.eagerPartialAborts + stats.eagerFullAborts);
    printf("Number of Commits = %lu\n", stats.numCommits);
#endif
}
