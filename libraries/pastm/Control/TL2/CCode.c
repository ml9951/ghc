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

#define LOCKED(x) (x & 1)

#define PASTM_SUCCESS              ((StgClosure*)(void*)&stg_PA_STM_SUCCESS_closure)
#define PASTM_FAIL                 ((StgClosure*)(void*)&stg_PA_STM_FAIL_closure)
#define NO_PTREC                   ((StgPTRecHeader *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER               &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER            &stg_PTREC_WITHOUTK_info
#define WRITESET_HEADER            &stg_WRITE_SET_info

static volatile unsigned long version_clock = 0;

//Initialize metadata
StgPTRecHeader * tl2_stmStartTransaction(Capability *cap, StgPTRecHeader * ptrec) {
    if(ptrec == NO_PTREC){
	ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
	ptrec->tail = TO_WITHOUTK(NO_PTREC);
	ptrec->capture_freq = 0;
	ptrec->numK = 0;
    }
    
    ptrec->read_set = TO_WITHOUTK(NO_PTREC);
    ptrec->lastK = TO_WITHK(NO_PTREC);
    ptrec->write_set = TO_WRITE_SET(NO_PTREC);
    ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);
    
    ptrec->read_version = version_clock;
    return ptrec;
}

static StgClosure * abort_tx(StgPTRecHeader * trec){
    trec->read_set = TO_WITHOUTK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->read_version = version_clock;
    return PASTM_FAIL;
}

StgClosure * tl2_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
			    StgTL2TVar * tvar, StgClosure * k){
    StgWriteSet * ws = trec->write_set;

    while(ws != TO_WRITE_SET(NO_PTREC)){
        if(TO_TL2(ws->tvar) == tvar){
            return ws->val;
        }
        ws = ws->next;
    }
    
    StgClosure * val; 
    unsigned long s1, s2;
    s1 = tvar->currentStamp;
    val = tvar->current_value;
    s2 = tvar->currentStamp;

    if(LOCKED(s1) || s1 != s2 || s1 > trec->read_version){
#ifdef STATS
	cap->pastmStats.eagerFullAborts++;
#endif
	return abort_tx(trec);
    }
    
    StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
    SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
    entry->tvar = TO_NOREC(tvar);
    entry->next = trec->read_set;
    entry->read_value = val; //TL2 doesn't use this, but if we don't set it the garbage collector will have issues
    trec->read_set = entry;
 
    return val; 
}

void tl2_stmWriteTVar(Capability *cap,
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
	tvar->currentStamp = tvar->oldStamp;
	ws = ws->next;
    }
}

/*
 * The write set is a pure linked list, such that if a transaction writes to the 
 * same tvar more than once, we keep both versions of it.  This is critical for 
 * partial abort, and probably isnt' such a bad idea for full abort either.  
 * It is nice to have the write set be purely functional so that we don't have 
 * to mark the entries as mutable.  When acquiring locks, if we find that we have
 * already locked a tvar in our write set, we drop it from the list.  Since we 
 * traverse in reverse chronological order, we know that the first one locked
 * is our latest modification.
 */
StgPTRecWithK * tl2_stmCommitTransaction(Capability *cap, StgPTRecHeader *trec, StgThreadID id) {
    unsigned long myStamp = trec->read_version;

    /*
    * lock with my thread ID shifted by one bit, with the last bit set.
    * this will let other threads know that the tvar is locked, but I 
    * will still be able to tell if I locked something.  
    */
    unsigned long lockVal = ((unsigned long)id << 1) | 1;
    
    //Acquire locks
    StgWriteSet * ws_ptr = trec->write_set;
    StgWriteSet ** trailer = &(trec->write_set);
    while(ws_ptr != TO_WRITE_SET(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ws_ptr->tvar);
	unsigned long stamp = tvar->currentStamp;

	if(stamp == lockVal){
	    *trailer = ws_ptr->next;
	    ws_ptr = ws_ptr->next;
	    continue;
	}

	if(LOCKED(stamp) || stamp > myStamp || cas((StgVolatilePtr)&(tvar->currentStamp), stamp, lockVal) != stamp){
	    releaseLocks(trec->write_set, ws_ptr);
	    return TO_WITHK(abort_tx(trec));
	}

	tvar->oldStamp = stamp;
	trailer = &(ws_ptr->next);
	ws_ptr = ws_ptr->next;
    }
    
    unsigned long write_version = atomic_inc(&version_clock, 2);

    //validate read set
    StgPTRecWithoutK * rs_ptr = trec->read_set;
    while(rs_ptr != TO_WITHOUTK(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(rs_ptr->tvar);
	unsigned long stamp = tvar->currentStamp;
	if((stamp <= myStamp && !LOCKED(stamp)) || stamp == lockVal){
	    rs_ptr = rs_ptr->next;
	}else{
#ifdef STATS
	    cap->pastmStats.commitTimeFullAborts++;
#endif
	    releaseLocks(trec->write_set, TO_WRITE_SET(NO_PTREC));
	    return TO_WITHK(abort_tx(trec));
	}
    }
    
    //push write set into global store
    ws_ptr = trec->write_set;
    while(ws_ptr != TO_WRITE_SET(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ws_ptr->tvar);
	tvar->current_value = ws_ptr->val;
	tvar->currentStamp = write_version;
	dirty_TL2_TVAR(cap, tvar);
	ws_ptr = ws_ptr->next;
    }

#ifdef STATS
    cap->pastmStats.numCommits++;
#endif

    return TO_WITHK(PASTM_SUCCESS);
}

void c_tl2_printSTMStats(){
#ifdef STATS
    StgPASTMStats stats = {0, 0, 0, 0, 0, 0, 0};
    getStats(&stats);
    printf("Commit Full Aborts = %lu\n", stats.commitTimeFullAborts);
    printf("Eager Full Aborts = %lu\n", stats.eagerFullAborts);
    printf("Total Aborts = %lu\n", stats.commitTimeFullAborts + stats.commitTimePartialAborts + 
	   stats.eagerPartialAborts + stats.eagerFullAborts);
    printf("Number of Commits = %lu\n", stats.numCommits);
#endif
}
