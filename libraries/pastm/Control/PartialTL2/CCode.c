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
    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
    trec->capture_freq = capture_freq | (capture_freq >> 32);
    trec->numK = 0;
}

static StgPTRecWithK * extendTS(StgPTRecHeader * trec){
 RETRY: 
    { 
	unsigned long newStamp = atomic_inc(&(version_clock), 1);
	unsigned long stamp = trec->read_version;
	StgPTRecWithoutK * ptr = trec->read_set;
	StgPTRecWithK * checkpoint = TO_WITHK(PASTM_SUCCESS);
	StgInt kCount = 0;
	
	while(ptr != TO_WITHOUTK(NO_PTREC)){
	    StgTL2TVar * tvar = TO_TL2(ptr->tvar);
	    unsigned long v1, v2;
	    if(ptr->header.info == WITHOUTK_HEADER){
		if(tvar->lock || tvar->stamp > stamp){
		    checkpoint = TO_WITHK(PASTM_FAIL);
		    kCount = 0;
		}
	    }else{
		StgPTRecWithK * withK = TO_WITHK(ptr);
		if(tvar->lock || tvar->stamp > stamp){
		    checkpoint = withK;
		    ptr = ptr->next;
		    kCount = 1;
		    continue;
		}else if(checkpoint == TO_WITHK(PASTM_FAIL)){
		    checkpoint = withK;
		}
		kCount++;
	    }
	    ptr = ptr->next;
	}
	
	trec->read_version = newStamp;
	if(checkpoint->header.info == WITHK_HEADER){
	    StgClosure * val;
	    StgTL2TVar * tvar = TO_TL2(checkpoint->tvar);
	    unsigned long stamp1, stamp2;
	    
	    while(cas((StgVolatilePtr)&(tvar->lock), 0, trec->read_version) && tvar->lock != stamp);
	    val = tvar->current_value;
	    stamp2 = tvar->stamp;
	    tvar->lock = 0;
	    
	    if(stamp2 > trec->read_version){
		clearTRec(trec);
		trec->read_version = atomic_inc(&version_clock, 1);
		return TO_WITHK(PASTM_FAIL);
	    }
	    
	    checkpoint->read_value = val;
	    trec->read_set = TO_WITHOUTK(checkpoint);
	    trec->write_set = checkpoint->write_set;
	    trec->lastK = checkpoint;
	    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
	    trec->capture_freq = capture_freq | (capture_freq >> 32);
	    trec->numK = kCount;
	    return checkpoint;
	}else if(checkpoint == TO_WITHK(PASTM_FAIL)){
	    clearTRec(trec);
	    return checkpoint;
	}else{
	    return checkpoint;
	}
    }
}

static StgPTRecWithK * commitValidate(StgPTRecHeader * trec){
 RETRY: 
    {
	unsigned long stamp = trec->read_version;
	StgPTRecWithoutK * ptr = trec->read_set;
	StgPTRecWithK * checkpoint = TO_WITHK(PASTM_SUCCESS);
	StgInt kCount = 0;
	
	while(ptr != TO_WITHOUTK(NO_PTREC)){
	    StgTL2TVar * tvar = TO_TL2(ptr->tvar);
	    if(ptr->header.info == WITHOUTK_HEADER){
		if((tvar->lock && tvar->lock != stamp) || tvar->stamp > stamp){
		    checkpoint = TO_WITHK(PASTM_FAIL);
		    kCount = 0;
		}
		ptr = ptr->next;
	    }else{ // WITHK
		StgPTRecWithK * withK = TO_WITHK(ptr);
		if((tvar->lock && tvar->lock != stamp) || tvar->stamp > stamp){
		    checkpoint = withK;
		    ptr = ptr->next;
		    kCount = 1;
		    continue;
		}else if(checkpoint == TO_WITHK(PASTM_FAIL)){
		    checkpoint = withK;
		}
		ptr = ptr->next;
		kCount++;
	    }
	}
	
	if(checkpoint->header.info == WITHK_HEADER){
	    StgClosure * val;
	    StgTL2TVar * tvar = TO_TL2(checkpoint->tvar);
	    
	    unsigned long stamp1, stamp2;

	    while(cas((StgVolatilePtr)&(tvar->lock), 0, trec->read_version) && tvar->lock != stamp);
	    val = tvar->current_value;
	    stamp2 = tvar->stamp;
	    tvar->lock = 0;
	    
	    if(stamp2 > trec->read_version){
		clearTRec(trec);
		trec->read_version = atomic_inc(&version_clock, 1);
		return TO_WITHK(PASTM_FAIL);
	    }

	    checkpoint->read_value = val;
	    trec->read_set = TO_WITHOUTK(checkpoint);
	    trec->write_set = checkpoint->write_set;
	    trec->lastK = checkpoint;
	    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
	    trec->capture_freq = capture_freq | (capture_freq >> 32);
	    trec->numK = kCount;
	    return checkpoint;
	}else if(checkpoint == TO_WITHK(PASTM_FAIL)){
	    clearTRec(trec);
	    return checkpoint;
	}else{
	    return checkpoint;
	}
    }
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
    
    /*
    StgClosure * val; 
    unsigned long stamp1, stamp2;
    
    while(cas((StgVolatilePtr)&(tvar->lock), 0, trec->read_version));
    val = tvar->current_value;
    stamp2 = tvar->stamp;
    tvar->lock = 0;

    if(stamp2 > trec->read_version){
	clearTRec(trec);
	trec->read_version = atomic_inc(&version_clock, 1);
	return PASTM_FAIL;
    }
    */
    
    unsigned long stamp1, stamp2;
    StgClosure * val;
 retry:
    do{
	stamp1 = tvar->stamp;
	val = tvar->current_value;
	stamp2 = tvar->stamp;
    }while(tvar->lock);

    if(stamp2 > trec->read_version || stamp1 != stamp2){
	StgPTRecWithK * res = extendTS(trec);
	if(res != TO_WITHK(PASTM_SUCCESS)){
	    return TO_CLOSURE(res);
	}
	goto retry;
    }
    
    if(trec->numK < KBOUND){//Still room for more
        if((trec->capture_freq & 0xFFFFFFFF) == 0){//Store the continuation
            StgPTRecWithK * entry = (StgPTRecWithK *)allocate(cap, sizeofW(StgPTRecWithK));
            SET_HDR(entry , &stg_PTREC_WITHK_info, CCS_SYSTEM);
            entry->tvar = TO_NOREC(tvar);
            entry->read_value = val;
            entry->next = trec->read_set;
            entry->write_set = trec->write_set;
            entry->continuation = k;
            entry->prev_k = trec->lastK;
            trec->read_set = TO_WITHOUTK(entry);
			
            trec->lastK = entry;
            trec->numK++;
            if(trec->numK == KBOUND)
                trec->capture_freq <<= 1; //double the frequency
            trec->capture_freq |= (trec->capture_freq >> 32);
            
        }else{//Don't store the continuation
            StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
            SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
            entry->tvar = TO_NOREC(tvar);
            entry->read_value = val;
            entry->next = trec->read_set;
            trec->read_set = entry;
            trec->capture_freq--;
        }
    }else{//filter the read set
        StgPTRecWithK * ptr = trec->lastK;
        int numK = trec->numK;
        while(ptr != TO_WITHK(NO_PTREC)){
            if(ptr->prev_k != TO_WITHK(NO_PTREC)){
                StgPTRecWithK * dropped = ptr->prev_k;
                dropped->continuation = TO_CLOSURE(NO_PTREC);
                ptr->prev_k = ptr->prev_k->prev_k;
                ptr = ptr->prev_k;
                SET_HDR(dropped, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
                numK--;
            }else{
                break;
            }
        }
        trec->numK = numK;
        StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
        SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
        entry->tvar = TO_NOREC(tvar);
        entry->read_value = val;
        entry->next = trec->read_set;
        trec->read_set = entry;
        trec->capture_freq--;
    }
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
 RETRY:
    {
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
		    StgPTRecWithK * res = extendTS(trec);
		    if(res != TO_WITHK(PASTM_SUCCESS)){
			return res;
		    }
		    goto RETRY;
		}
	    }
	    trailer = &(ptr->next);
	    ptr = ptr->next;
	}
    
	unsigned long write_version = atomic_inc(&version_clock, 1);

	//validate read set
	ptr = trec->write_set;
	StgPTRecWithK * res = commitValidate(trec);
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
    }

#ifdef STATS
    atomic_inc((StgVolatilePtr)&(stats.commitTimeFullAborts), cap->pastmStats.commitTimeFullAborts);
    atomic_inc((StgVolatilePtr)&(stats.commitTimePartialAborts), cap->pastmStats.commitTimePartialAborts);
    atomic_inc((StgVolatilePtr)&(stats.eagerFullAborts), cap->pastmStats.eagerFullAborts);
    atomic_inc((StgVolatilePtr)&(stats.eagerPartialAborts), cap->pastmStats.eagerPartialAborts);
    atomic_inc((StgVolatilePtr)&(stats.numCommits), 1);
    cap->pastmStats.commitTimeFullAborts = 0;
    cap->pastmStats.commitTimePartialAborts = 0;
    cap->pastmStats.eagerFullAborts = 0;
    cap->pastmStats.eagerPartialAborts = 0;
#endif

    return TO_WITHK(PASTM_SUCCESS);
}

void pa_printSTMStats(){
#ifdef STATS
    printf("Commit Full Aborts = %lu\n", stats.commitTimeFullAborts);
    printf("Commit Partial Aborts = %lu\n", stats.commitTimePartialAborts);
    printf("Eager Full Aborts = %lu\n", stats.eagerFullAborts);
    printf("Eager Partial Aborts = %lu\n", stats.eagerPartialAborts);
    printf("Total Commit Time Aborts = %lu\n", stats.commitTimeFullAborts + stats.commitTimePartialAborts);
    printf("Total Eager Aborts = %lu\n", stats.eagerFullAborts + stats.eagerPartialAborts);
    printf("Total Full Aborts = %lu\n", stats.commitTimeFullAborts + stats.eagerFullAborts);
    printf("Total Partial Aborts = %lu\n", stats.commitTimePartialAborts + stats.eagerPartialAborts);
    printf("Total Aborts = %lu\n", stats.commitTimeFullAborts + stats.commitTimePartialAborts + 
	   stats.eagerPartialAborts + stats.eagerFullAborts);
    printf("Number of Commits = %lu\n", stats.numCommits);
#endif
}
