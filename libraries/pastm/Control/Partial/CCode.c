#include "Rts.h"

//#include "PartialAbortSTM.h"
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


#define PASTM_SUCCESS              ((StgClosure*)(void*)&stg_PA_STM_SUCCESS_closure)
#define PASTM_FAIL                 ((StgClosure*)(void*)&stg_PA_STM_FAIL_closure)
#define NO_PTREC                   ((StgPTRecHeader *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER               &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER            &stg_PTREC_WITHOUTK_info
#define WRITESET_HEADER            &stg_WRITE_SET_info

#define TRACE(_x...) printf(_x)
//debugTrace(DEBUG_stm, "STM: " _x) //supply "+RTS -Dm" for STM trace output 


static volatile unsigned long version_clock = 0;

StgPTRecHeader * pa_stmStartTransaction(Capability *cap, StgPTRecHeader * ptrec) {
  
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

static StgPTRecWithK * pa_validate(StgPTRecHeader * trec, Capability * cap){
    while(TRUE){
        unsigned long time = version_clock;
        if((time & 1) != 0){
            continue; //clock is locked
        }
        trec->read_version = time;
        //validate read set
        StgPTRecWithoutK * ptr = trec->read_set;
        StgPTRecWithK *checkpoint = TO_WITHK(PASTM_SUCCESS);
        StgInt kCount = 0;
        while(ptr != TO_WITHOUTK(NO_PTREC)){
            if(ptr->header.info == WITHK_HEADER){ // this is a WithK entry
                StgPTRecWithK * withK = TO_WITHK(ptr);
                if(withK->read_value != withK->tvar->current_value){
                    checkpoint = TO_WITHK(PASTM_FAIL);
                    ptr = ptr->next;
                    kCount = 0;
                    continue;
                }else if(checkpoint == TO_WITHK(PASTM_FAIL)){ //Valid and we need a checkpoint
                    checkpoint = withK;
                }
                kCount++;
            }else if(ptr->read_value != ptr->tvar->current_value){  //WithoutK entry
                checkpoint = TO_WITHK(PASTM_FAIL);
                kCount = 0;
            }
            ptr = ptr->next;
        }
        if(checkpoint == TO_WITHK(PASTM_FAIL)){ //no checkpoint found, but we need to abort
            return checkpoint;
        }
        if(checkpoint != TO_WITHK(PASTM_SUCCESS)){ //validation failed, but we found a checkpoint
            //try reading from this tvar
            StgTVar * tvar = checkpoint->tvar;
            StgClosure * val = tvar->current_value;
            if(time == version_clock){
                checkpoint->read_value = val; //apply the continuation to this in C--
                trec->read_set = TO_WITHOUTK(checkpoint);
                trec->write_set = checkpoint->write_set;
                trec->lastK = checkpoint;
                StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
                trec->capture_freq = capture_freq | (capture_freq >> 32);
                trec->numK = kCount;
                return checkpoint;
            }else{
                continue; //revalidate the whole log
            }
            
        }

        if(time == version_clock){
		  TRACE("%d: Committing transaction, Started at time %lu, version_clock = %lu\n", cap->no, time, version_clock);
            return checkpoint; //necessarily PASTM_SUCCESS
        }
        //Validation succeeded, but someone else committed in the meantime, loop back around...
    }
}

void printStats(Capability * cap){
  printf("%d: eager partial = %lu, eager full = %lu, commit time partial = %lu, commit time full = %lu\n", 
		 cap->no, cap->pastmStats.eagerPartialAborts, cap->pastmStats.eagerFullAborts,
		 cap->pastmStats.commitTimePartialAborts, cap->pastmStats.commitTimeFullAborts);
}

StgClosure * pa_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
							StgTVar * tvar, StgClosure * k){
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
	  StgPTRecWithK * checkpoint = pa_validate(trec, cap);
        if(checkpoint != TO_WITHK(PASTM_SUCCESS)){
		    if(checkpoint == TO_WITHK(PASTM_FAIL)){
#ifdef STATS
			  cap->pastmStats.eagerFullAborts++;
			  printStats(cap);
#endif
			  return TO_CLOSURE(checkpoint);
			}
#ifdef STATS
			cap->pastmStats.eagerPartialAborts++;
            printStats(cap);
#endif 
			return TO_CLOSURE(checkpoint);
        }
        val = tvar->current_value;
    }

    if(trec->numK < KBOUND){//Still room for more
        if((trec->capture_freq & 0xFFFFFFFF) == 0){//Store the continuation
            StgPTRecWithK * entry = (StgPTRecWithK *)allocate(cap, sizeofW(StgPTRecWithK));
            SET_HDR(entry , &stg_PTREC_WITHK_info, CCS_SYSTEM);
            entry->tvar = tvar;
            entry->read_value = val;
            entry->next = trec->read_set;
            entry->write_set = trec->write_set;
            entry->continuation = k;
            entry->prev_k = trec->lastK;
            entry->is_retry = FALSE;
            trec->read_set = TO_WITHOUTK(entry);
			
            trec->lastK = entry;
            trec->numK++;
            if(trec->numK == KBOUND)
                trec->capture_freq <<= 1; //double the frequency
            trec->capture_freq |= (trec->capture_freq >> 32);
            
        }else{//Don't store the continuation
            StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
            SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
            entry->tvar = tvar;
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
        entry->tvar = tvar;
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

StgPTRecWithK * pa_stmCommitTransaction(Capability *cap, StgPTRecHeader *trec) {
    unsigned long snapshot = trec->read_version;
    while (cas(&version_clock, snapshot, snapshot+1) != snapshot){ 
	    StgPTRecWithK * checkpoint = pa_validate(trec, cap);
        if(checkpoint != (StgPTRecWithK *) PASTM_SUCCESS){
		    if(checkpoint == TO_WITHK(PASTM_FAIL)){
#ifdef STATS
			  printStats(cap);
			    cap->pastmStats.commitTimeFullAborts++;
#endif
				return checkpoint;
			} 
#ifdef STATS
			cap->pastmStats.commitTimePartialAborts++;
			printStats(cap);
#endif
			return checkpoint;
        }
        snapshot = trec->read_version;
    }
    
	TRACE("%d: Locked clock, currently value is %lu\n", cap->no, version_clock);

    StgWriteSet * write_set = trec->write_set;
	while(write_set != TO_WRITE_SET(NO_PTREC)){
        StgTVar * tvar = write_set->tvar;
        tvar->current_value = write_set->val;
        dirty_TVAR(cap,tvar);
        write_set = write_set->next;
    }
#ifdef STATS
	cap->pastmStats.numCommits++;
	printStats(cap);
#endif
    version_clock = snapshot + 2;//unlock clock
    return (StgPTRecWithK *)PASTM_SUCCESS;
}
