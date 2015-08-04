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

StgClosure * abort_transaction(StgPTRecHeader * trec){
    trec->read_version = atomic_inc(&version_clock, 1);
    clearTRec(trec);
    return PASTM_FAIL;
}

static StgPTRecWithK * extendTS(StgPTRecHeader * trec, Capability * cap){
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
	StgTL2TVar * tvar = TO_TL2(checkpoint->tvar);

	StgClosure * val; 
	unsigned long stamp1, stamp2;
	    
	stamp1 = tvar->stamp;
	if(tvar->lock || stamp1 > trec->read_version){
	    clearTRec(trec);
	    return TO_WITHK(PASTM_FAIL);
	}
	    
	val = tvar->current_value;
	    
	stamp2 = tvar->stamp;
	if(tvar->lock || stamp1 != stamp2){
	    clearTRec(trec);
	    return TO_WITHK(PASTM_FAIL);
	}
	    
#ifdef STATS
	cap->pastmStats.eagerPartialAborts++;
#endif

	checkpoint->read_value = val;
	trec->read_set = TO_WITHOUTK(checkpoint);
	trec->write_set = checkpoint->write_set;
	trec->lastK = checkpoint;
	StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
	trec->capture_freq = capture_freq | (capture_freq >> 32);
	trec->numK = kCount;
	return checkpoint;
    }else if(checkpoint == TO_WITHK(PASTM_FAIL)){
#ifdef STATS
	cap->pastmStats.eagerFullAborts++;
#endif
	clearTRec(trec);
	return checkpoint;
    }else{
#ifdef STATS
	cap->pastmStats.tsExtensions++;
#endif
	return checkpoint;
    }
    
}

static StgPTRecWithK * commitValidate(StgPTRecHeader * trec, Capability * cap){
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
	StgTL2TVar * tvar = TO_TL2(checkpoint->tvar);
	    

	StgClosure * val; 
	unsigned long stamp1, stamp2;
    
	if(tvar->lock == stamp){
	    val = tvar->current_value;
	}else{
	    stamp1 = tvar->stamp;
	    if(tvar->lock || stamp1 > trec->read_version)
		return TO_WITHK(abort_transaction(trec));
	    
	    val = tvar->current_value;
	    
	    stamp2 = tvar->stamp;
	    if(tvar->lock || stamp1 != stamp2)
		return TO_WITHK(abort_transaction(trec));
	}
	
#ifdef STATS
	cap->pastmStats.commitTimePartialAborts++;
#endif

	checkpoint->read_value = val;
	trec->read_set = TO_WITHOUTK(checkpoint);
	trec->write_set = checkpoint->write_set;
	trec->lastK = checkpoint;
	StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
	trec->capture_freq = capture_freq | (capture_freq >> 32);
	trec->numK = kCount;
	return checkpoint;
    }else if(checkpoint == TO_WITHK(PASTM_FAIL)){
#ifdef STATS
	cap->pastmStats.commitTimeFullAborts++;
#endif
	clearTRec(trec);
	return checkpoint;
    }else{
#ifdef STATS
	cap->pastmStats.numCommits++;
#endif
	return checkpoint;
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

    StgClosure * val; 
    unsigned long stamp1, stamp2;

 retry:
    stamp1 = tvar->stamp;
    if(tvar->lock || stamp1 > trec->read_version){
	StgPTRecWithK * res = extendTS(trec, cap);
	if(res != TO_WITHK(PASTM_SUCCESS)){
	    return TO_CLOSURE(res);
	}
	goto retry;
    }
    
    val = tvar->current_value;
    
    stamp2 = tvar->stamp;
    if(tvar->lock || stamp1 != stamp2){
	StgPTRecWithK * res = extendTS(trec, cap);
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
		    StgPTRecWithK * res = extendTS(trec, cap);
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

	if(version_clock != trec->read_version + 1){
	    //validate read set
	    ptr = trec->write_set;
	    StgPTRecWithK * res = commitValidate(trec, cap);
	    if(res != TO_WITHK(PASTM_SUCCESS)){
		releaseLocks(ptr, TO_WRITE_SET(NO_PTREC));
		trec->read_version = write_version;
		return res;
	    }
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

    return TO_WITHK(PASTM_SUCCESS);
}

void pa_printSTMStats(){
#ifdef STATS
    StgPASTMStats stats = {0, 0, 0, 0, 0, 0, 0};
    getStats(&stats);
    
    printf("Commit Full Aborts = %lu\n", stats.commitTimeFullAborts);
    printf("Commit Partial Aborts = %lu\n", stats.commitTimePartialAborts);
    printf("Eager Full Aborts = %lu\n", stats.eagerFullAborts);
    printf("Eager Partial Aborts = %lu\n", stats.eagerPartialAborts);
    printf("Timestamp Extensions = %lu\n", stats.tsExtensions);
    printf("Total Aborts = %lu\n", stats.commitTimeFullAborts + stats.commitTimePartialAborts + 
	   stats.eagerPartialAborts + stats.eagerFullAborts);
    printf("Number of Commits = %lu\n", stats.numCommits);
#endif
}
