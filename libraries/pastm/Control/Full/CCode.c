#include "Rts.h"

//#include "PartialAbortSTM.h"
//#include "Trace.h"
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

StgPTRecHeader * fa_stmStartTransaction(Capability *cap) {
  
    StgPTRecHeader * ptrec;
    ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
    SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
    
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

static StgClosure * fa_validate(StgPTRecHeader * trec){
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

StgClosure * fa_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
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
        StgClosure * res = fa_validate(trec);
        if(res == PASTM_FAIL){
#ifdef STATS
	    cap->pastmStats.eagerFullAborts++;
#endif
            return PASTM_FAIL;
        }
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

void fa_stmWriteTVar(Capability *cap,
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

StgClosure * fa_stmCommitTransaction(Capability *cap, StgPTRecHeader *trec) {
    unsigned long snapshot = trec->read_version;
    while (cas(&version_clock, snapshot, snapshot+1) != snapshot){ 
        StgClosure * res = fa_validate(trec);
        if(res != PASTM_SUCCESS){
#ifdef STATS
	    cap->pastmStats.commitTimeFullAborts++;
#endif
            //The validate function sets up the trec with the appropriate read/write sets
            return res;
        }
        snapshot = trec->read_version;
    }
   
    StgWriteSet * write_set = trec->write_set;
    while(write_set != TO_WRITE_SET(NO_PTREC)){
        StgTVar * tvar = write_set->tvar;
        tvar->current_value = write_set->val;
        dirty_TVAR(cap,tvar);
        write_set = write_set->next;
    }

#ifdef STATS
    stats.commitTimeFullAborts += cap->pastmStats.commitTimeFullAborts;
    stats.eagerFullAborts += cap->pastmStats.eagerFullAborts;
    stats.numCommits++;
    cap->pastmStats.commitTimeFullAborts = 0;
    cap->pastmStats.eagerFullAborts = 0;
#endif

    version_clock = snapshot + 2;//unlock clock
    return PASTM_SUCCESS;
}

void fa_printSTMStats(){
#ifdef STATS
    printf("Commit Full Aborts   : %lu\n", stats.commitTimeFullAborts);
    printf("Eager Full Aborts    : %lu\n", stats.eagerFullAborts);
    printf("Number of Commits    : %lu\n", stats.numCommits);
#endif
}



