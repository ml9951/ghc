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

StgPTRecHeader * ptl2_stmStartTransaction(Capability *cap, StgPTRecHeader * ptrec){
    if(ptrec == NO_PTREC){
	ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
	ptrec->tail = TO_WITHOUTK(NO_PTREC);
    }
    
    ptrec->read_set = TO_WITHOUTK(NO_PTREC);
    ptrec->lastK = TO_WITHK(NO_PTREC);
    ptrec->write_set = TO_WRITE_SET(NO_PTREC);
    ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);
    
    ptrec->read_version = version_clock;

    ptrec->capture_freq = ((unsigned long)START_FREQ << 32) + START_FREQ ; 
    ptrec->numK = 0;

    return ptrec;
}

static void clearTRec(StgPTRecHeader * trec){
    trec->read_set = TO_WITHOUTK(NO_PTREC);
    trec->lastK = TO_WITHK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->retry_stack = TO_OR_ELSE(NO_PTREC);
    trec->read_version = version_clock;
    //TODO: we should probably use bitfields instead of manually messing with the bits
    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
    trec->capture_freq = capture_freq | (capture_freq >> 32);
    trec->numK = 0;
}

void releaseLocks(StgWriteSet * ws, StgWriteSet * sentinel){
    while(ws != sentinel){
	StgTL2TVar * tvar = TO_TL2(ws->tvar);
	tvar->currentStamp = tvar->oldStamp;
	ws = ws->next;
    }
}

/*
 * validate the read set.  If validation fails, then the trec will have been set up 
 * appropriately for an abort.  This also updates the read_version, so if this is being
 * called at commit time, the read_version should be used to overwrite the tvar stamps 
 * when pushing the write set into the global store.  The "eager" parameter determines
 * the context in which this is being called.  If it is NOT eager, then we should unlock
 * the write set if a violation is detected.   
 * If calling validate eagerly, supply 0 (or any even number) as the lockVal, this way 
 * LOCKED(s) and s == lockVal are mutually exclusive.
 */
static StgPTRecWithK * validate(StgPTRecHeader * trec, Capability * cap, StgBool eager, unsigned long lockVal){
    unsigned long newStamp = atomic_inc(&(version_clock), 2);
    unsigned long myStamp = trec->read_version;
    StgPTRecWithoutK * ptr = trec->read_set;
    StgPTRecWithK * checkpoint = TO_WITHK(PASTM_SUCCESS);
    StgInt kCount = 0;

 RETRY:
    while(ptr != TO_WITHOUTK(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ptr->tvar);
	unsigned long s = tvar->currentStamp;

	if(ptr->header.info == WITHOUTK_HEADER){
	    if((s > myStamp || LOCKED(s)) && s != lockVal){
		checkpoint = TO_WITHK(PASTM_FAIL);
		kCount = 0;
	    }
	}else{
	    if((s > myStamp || LOCKED(s)) && s != lockVal){
		checkpoint = TO_WITHK(ptr);
		kCount = 0;
	    }else if(checkpoint == TO_WITHK(PASTM_FAIL)){
		checkpoint = TO_WITHK(ptr);
	    }
	    kCount++;
	}
	ptr = ptr->next;
    }

    trec->read_version = newStamp;
    if(checkpoint->header.info == WITHK_HEADER){     //Something is out of date, but we found a checkpoint
	if(!eager){ //we have acquired locks, so release them
	    releaseLocks(trec->write_set, TO_WRITE_SET(NO_PTREC));
	    trec->write_set = TO_WRITE_SET(NO_PTREC);
	}
	
	StgTL2TVar * tvar = TO_TL2(checkpoint->tvar);
	
	StgClosure * val; 
	unsigned long stamp1, stamp2;
	
	stamp1 = tvar->currentStamp;
	val = tvar->current_value;
	stamp2 = tvar->currentStamp;

	if(LOCKED(stamp1) || stamp1 != stamp2 || stamp1 > newStamp){
	    //try validating again, but don't go past this checkpoint
	    myStamp = newStamp;
	    newStamp = atomic_inc(&version_clock, 2);
	    ptr = TO_WITHOUTK(checkpoint);
	    kCount = 0;
	    goto RETRY;
	}
	
#ifdef STATS
	if(eager)
	    cap->pastmStats.eagerPartialAborts++;
	else
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
    }else if(checkpoint == TO_WITHK(PASTM_FAIL)){     //Something is out of date, but no checkpoint was found
#ifdef STATS
	if(eager)
	    cap->pastmStats.eagerFullAborts++;
	else
	    cap->pastmStats.commitTimeFullAborts++;
#endif
	if(!eager)
	    releaseLocks(trec->write_set, TO_WRITE_SET(NO_PTREC));
	clearTRec(trec);
	return checkpoint;
    }else{                                            //everything is still valid, checkpoint must be PASTM_SUCCESS
	return checkpoint;
    }
}


StgClosure * ptl2_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
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
    stamp1 = tvar->currentStamp;
    val = tvar->current_value;
    stamp2 = tvar->currentStamp;

    if(LOCKED(stamp1) || stamp1 != stamp2 || stamp1 > trec->read_version){
	StgPTRecWithK * res = validate(trec, cap, TRUE, 0);
	if(res != TO_WITHK(PASTM_SUCCESS)){
	    return TO_CLOSURE(res);
	}
	//timestamp was extended, try reading again
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

void ptl2_stmWriteTVar(Capability *cap,
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

StgPTRecWithK * ptl2_stmCommitTransaction(Capability *cap, StgPTRecHeader *trec, StgThreadID id) {
    
    unsigned long myStamp = trec->read_version;
    unsigned long lockVal = ((unsigned long) id << 1) | 1;
    
    //Acquire locks
    StgWriteSet * ws_ptr = trec->write_set;
    StgWriteSet ** trailer = &(trec->write_set);
    while(ws_ptr != TO_WRITE_SET(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ws_ptr->tvar);
	unsigned long stamp = tvar->currentStamp;

	if(stamp == lockVal){ //we locked this, so drop it from the write set
	    *trailer = ws_ptr->next;
	    ws_ptr = ws_ptr->next;
	    continue;
	}

	if(LOCKED(stamp) || stamp > myStamp || cas((StgVolatilePtr)&(tvar->currentStamp), stamp, lockVal) != stamp){
	    releaseLocks(trec->write_set, ws_ptr);
	    clearTRec(trec);
	    return TO_WITHK(PASTM_FAIL);
	}

	tvar->oldStamp = stamp;
	trailer = &(ws_ptr->next);
	ws_ptr = ws_ptr->next;
    }
    
    StgPTRecWithK * res = validate(trec, cap, FALSE, lockVal);
    if(res != TO_WITHK(PASTM_SUCCESS)){
	return res;
    }
    
    unsigned long write_version = trec->read_version; //validate updates trec->read_version

    //push write set into global store
    ws_ptr = trec->write_set;
    while(ws_ptr != TO_WRITE_SET(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ws_ptr->tvar);
	tvar->current_value = ws_ptr->val;
	tvar->currentStamp = write_version;
	dirty_TL2_TVAR(cap, tvar);
	ws_ptr = ws_ptr->next;
    }
    return TO_WITHK(PASTM_SUCCESS);
}

void c_ptl2_printSTMStats(){
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
#endif
}
