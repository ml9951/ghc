#include "Rts.h"
#include "rts/Threads.h"
#include "sm/Storage.h"
#include <stdio.h>
#include "eventlog/EventLog.h"

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

#define PASTM_SUCCESS              ((StgClosure*)(void*)&stg_PA_STM_SUCCESS_closure)
#define PASTM_FAIL                 ((StgClosure*)(void*)&stg_PA_STM_FAIL_closure)
#define NO_PTREC                   ((StgPTRecHeader *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER               &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER            &stg_PTREC_WITHOUTK_info
#define WRITESET_HEADER            &stg_WRITE_SET_info

static volatile unsigned long version_clock = 0;

StgPTRecHeader * norec_stmStartTransaction(Capability *cap, StgPTRecHeader * ptrec) {
#ifdef TRACING  
    postEvent (cap, EVENT_START_TX);
#endif
    if(ptrec == NO_PTREC){
	ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
    }

    ptrec->read_set = TO_WITHOUTK(NO_PTREC);
    ptrec->lastK = TO_WITHK(NO_PTREC);
    ptrec->write_set = TO_WRITE_SET(NO_PTREC);

    ptrec->tail = TO_WITHOUTK(NO_PTREC);

    ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);

    //get a read version
    ptrec->read_version = version_clock;
    while((ptrec->read_version & 1) != 0){
        ptrec->read_version = version_clock;
    }
    ptrec->capture_freq = ((unsigned long)START_FREQ << 32) + START_FREQ ; 
    ptrec->numK = 0;

    return ptrec;
}

void clearTRec(StgPTRecHeader * trec){
    trec->read_set = TO_WITHOUTK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
}

static StgClosure * norec_validate(Capability * cap, StgPTRecHeader * trec){
#ifdef TRACING
    postEvent (cap, EVENT_BEGIN_COMMIT);
#endif
    while(TRUE){
        unsigned long time = version_clock;
        if((time & 1) != 0){
            continue; //clock is locked
        }

        trec->read_version = time;
        //validate read set
        StgPTRecWithoutK * ptr = trec->read_set;

        while(ptr != TO_WITHOUTK(NO_PTREC)){
	    if(ptr->read_value != ptr->tvar->current_value){
		clearTRec(trec);
		return PASTM_FAIL;
	    }
	    ptr = ptr->next;
	}
        if(time == version_clock){
	    return PASTM_SUCCESS; //necessarily PASTM_SUCCESS
        }
        //Validation succeeded, but someone else committed in the meantime, loop back around...
    }
}

StgClosure * norec_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
			    StgTVar * tvar){
    StgWriteSet * ws = trec->write_set;

    while(ws != TO_WRITE_SET(NO_PTREC)){
        if(ws->tvar == tvar){
            return ws->val;
        }
        ws = ws->next;
    }
    
    //Not found in write set
    StgClosure * val = tvar->current_value;
    while(trec->read_version != version_clock){
        StgClosure * res = norec_validate(cap, trec);
        if(res == PASTM_FAIL){
#ifdef STATS
	    cap->pastmStats.eagerFullAborts++;
#endif
#ifdef TRACING  
	    postEvent (cap, EVENT_EAGER_FULL_ABORT);
#endif
            return PASTM_FAIL;
        }
#ifdef STATS
	cap->pastmStats.tsExtensions++;
#endif
        val = tvar->current_value;
    }
    
    StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
    SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);

    entry->tvar = tvar;
    entry->read_value = val;
    entry->next = trec->read_set;
    trec->read_set = entry;
    return val;
}

void norec_stmWriteTVar(Capability *cap,
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

StgClosure * norec_stmCommitTransaction(Capability *cap, StgPTRecHeader *trec) {
    unsigned long snapshot = trec->read_version;
    while (cas(&version_clock, snapshot, snapshot+1) != snapshot){ 
        StgClosure * res = norec_validate(cap, trec);
        if(res != PASTM_SUCCESS){
#ifdef STATS
	    cap->pastmStats.commitTimeFullAborts++;
#endif
#ifdef TRACING  
	    postEvent (cap, EVENT_COMMIT_FULL_ABORT);
#endif
            //The validate function sets up the trec with the appropriate read/write sets
            return res;
        }
        snapshot = trec->read_version;
    }

    //reverse the write set, so that new modifications overwrite old ones
    StgWriteSet * one, * two;
    one = TO_WRITE_SET(NO_PTREC);
    two = trec->write_set;
    while(two != TO_WRITE_SET(NO_PTREC)){
        StgWriteSet * temp = two->next;
	two->next = one;
	one = two;
	two = temp;
    }

    StgWriteSet * write_set = one;
    while(write_set != TO_WRITE_SET(NO_PTREC)){
        StgTVar * tvar = write_set->tvar;
        tvar->current_value = write_set->val;
        dirty_TVAR(cap,tvar);
        write_set = write_set->next;
    }

#ifdef STATS
    cap->pastmStats.numCommits++;
#endif

    version_clock = snapshot + 2;//unlock clock

#ifdef TRACING  
    postEvent (cap, EVENT_COMMIT_TX);
#endif
    
    return PASTM_SUCCESS;
}

void c_norec_printSTMStats(){
#ifdef STATS
    StgPASTMStats stats = {0, 0, 0, 0, 0, 0, 0};
    getStats(&stats);

    printf("Commit Full Aborts = %lu\n", stats.commitTimeFullAborts);
    printf("Eager Full Aborts = %lu\n", stats.eagerFullAborts);
    printf("Total Aborts = %lu\n", stats.commitTimeFullAborts + stats.eagerFullAborts);
    printf("Timestamp Extensions = %lu\n", stats.tsExtensions);
    printf("Number of Commits = %lu\n", stats.numCommits);
#endif
}



