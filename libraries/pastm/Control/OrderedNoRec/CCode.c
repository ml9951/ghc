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

#define PASTM_SUCCESS              ((StgClosure*)(void*)&stg_PA_STM_SUCCESS_closure)
#define PASTM_FAIL                 ((StgClosure*)(void*)&stg_PA_STM_FAIL_closure)
#define NO_PTREC                   ((StgPTRecHeader *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER               &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER            &stg_PTREC_WITHOUTK_info
#define WRITESET_HEADER            &stg_WRITE_SET_info

#define TRACE(_x...) debugTrace(DEBUG_stm, "STM: " _x)

static volatile unsigned long version_clock = 0;

static StgTVar dummyTV = {.header = {.info = &stg_NO_PTREC_info /*, TODO: add profiling field*/}, 
                          .current_value = PASTM_SUCCESS, //dummy value
                          .first_watch_queue_entry = ((StgTVarWatchQueue *)(void *)&stg_END_STM_WATCH_QUEUE_closure),
                          .num_updates = 0};

StgPTRecHeader * ord_stmStartTransaction(Capability *cap, StgPTRecHeader * ptrec) {
    if(ptrec == NO_PTREC){
	ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);

	ptrec->lastK = TO_WITHK(NO_PTREC);
	ptrec->write_set = TO_WRITE_SET(NO_PTREC);
	ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);
    }
    
    StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
    SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
	
    entry->tvar = &dummyTV;
    entry->read_value = PASTM_SUCCESS;
    entry->next = TO_WITHOUTK(NO_PTREC);

    ptrec->read_set = entry;
    ptrec->tail = entry;

    //get a read version
    ptrec->read_version = version_clock;
    while((ptrec->read_version & 1) != 0){
        ptrec->read_version = version_clock;
    }
    ptrec->capture_freq = ((unsigned long)START_FREQ << 32) + START_FREQ ; 
    ptrec->numK = 0;

    return ptrec;
}

static inline void clearTRec(StgPTRecHeader * trec){
    StgPTRecWithoutK * first = trec->read_set;
    first->next = TO_WITHOUTK(NO_PTREC);
    trec->lastK = TO_WITHK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->retry_stack = TO_OR_ELSE(NO_PTREC);
    trec->tail = first;
    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
    trec->capture_freq = capture_freq | (capture_freq >> 32);
    trec->numK = 0;
}

static void sanity(StgPTRecHeader * trec){
    int freq = trec->capture_freq >> 32;
    int counter = freq;

    StgPTRecWithoutK * ptr = trec->read_set;
    while(ptr != TO_WITHOUTK(NO_PTREC)){
	if(counter == 0 && ptr->header.info != WITHK_HEADER){
	    if(ptr->header.info == WITHK_HEADER){
		counter = freq;
	    }else{
		printf("checkpoint at incorrect interval\n");
	    }
	}

	if(counter != 0 && ptr->header.info == WITHK_HEADER){
	    printf("checkpoint at incorrect interval\n");
	}
	
	ptr = ptr->next;
	counter--;

    }
    
    
}

static StgPTRecWithK * ord_validate(StgPTRecHeader * trec, Capability * cap){
 RETRY:
    while(TRUE){
        unsigned long time = version_clock;
        if((time & 1) != 0){
            continue; //clock is locked
        }
        trec->read_version = time;
        //validate read set
        StgPTRecWithoutK * ptr = trec->read_set; //first element is the dummy
        StgPTRecWithK *checkpoint = NULL;
        StgInt kCount = 0;
	
	int i = 0;
	while(ptr != TO_WITHOUTK(NO_PTREC)){
	    if(ptr->read_value != ptr->tvar->current_value){
		if(checkpoint != NULL){ //we have a checkpoint
		    //try reading from this tvar
		    StgClosure * val = checkpoint->tvar->current_value;
		    if(time == version_clock){
			checkpoint->read_value = val; //apply the continuation to this in C--
			checkpoint->next = TO_WITHOUTK(NO_PTREC);
			
			cap->pastmStats.fastForwardAttempts += i;

			trec->tail = TO_WITHOUTK(checkpoint);
			trec->write_set = checkpoint->write_set;
			trec->lastK = checkpoint;
			StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
			trec->capture_freq = capture_freq | (capture_freq >> 32);
			trec->numK = kCount;
			return checkpoint;
		    }else{
			goto RETRY;
		    }
		}else{//checkpoint == NULL
		    clearTRec(trec);
		    return TO_WITHK(PASTM_FAIL);
		}
	    }
	    if(ptr->header.info == WITHK_HEADER){//entry is valid and we have a checkpoint
		checkpoint = TO_WITHK(ptr);
		kCount++;
	    }
	    i++;
	    ptr = ptr->next;
	}
	
        if(time == version_clock){
            return TO_WITHK(PASTM_SUCCESS);
        }
#ifdef STATS
	cap->pastmStats.tsExtensions++;
#endif
        //Validation succeeded, but someone else committed in the meantime, loop back around...
    }
}

StgClosure * ord_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
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
	StgPTRecWithK * checkpoint = ord_validate(trec, cap);
        if(checkpoint != TO_WITHK(PASTM_SUCCESS)){
#ifdef STATS
	    if(checkpoint == TO_WITHK(PASTM_FAIL)){
		cap->pastmStats.eagerFullAborts++;
		return TO_CLOSURE(checkpoint);
	    }
	    cap->pastmStats.eagerPartialAborts++;
#endif
	    return TO_CLOSURE(checkpoint);
        }
#ifdef STATS
	cap->pastmStats.tsExtensions++;
#endif
        val = tvar->current_value;
    }
    
    if(trec->numK < KBOUND){//Still room for more
        if((trec->capture_freq & 0xFFFFFFFF) == 0){//Store the continuation
            StgPTRecWithK * entry = (StgPTRecWithK *)allocate(cap, sizeofW(StgPTRecWithK));
            SET_HDR(entry , &stg_PTREC_WITHK_info, CCS_SYSTEM);
            entry->tvar = tvar;
            entry->read_value = val;
            entry->next = TO_WITHOUTK(NO_PTREC);
            entry->write_set = trec->write_set;
            entry->continuation = k;
            entry->prev_k = trec->lastK;
	    
	    trec->tail->next = TO_WITHOUTK(entry); //append to end
	    /*bdescr * desc = Bdescr((StgPtr)trec->tail);
	    if(desc->gen_no > 0 && trec->tail->header.info != &stg_PTREC_WITHOUTK_MUT_info){
		//printf("Adding to remember set, tail is in generation %d\n", desc->gen_no);
		cap->pastmStats.fastForwards++;
		recordClosureMutated(cap, (StgClosure*)trec->tail);
	    }
*/

            trec->tail = TO_WITHOUTK(entry);
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
            entry->next = TO_WITHOUTK(NO_PTREC);
	    
	    trec->tail->next = entry; //append to end
	    /*    bdescr * desc = Bdescr((StgPtr)trec->tail);
	    if(desc->gen_no > 0 && trec->tail->header.info != &stg_PTREC_WITHOUTK_MUT_info){
		//	printf("Adding to remember set, tail is in generation %d\n", desc->gen_no);
		cap->pastmStats.fastForwards++;
		recordClosureMutated(cap, (StgClosure*)trec->tail);
	    }
	    */
	    trec->tail = entry;
            trec->capture_freq--;
        }
    }else{//filter the read set
        StgPTRecWithK * ptr = trec->lastK;
        int numK = trec->numK;
	/* Note that we are mutating the prev_k field here, however, there
	 * is no need to mess with dirty/clean headers or make these have a 
	 * mutable header type.  Since we update prev_k to always point to
	 * somthing deeper in the list, we know that we will never end up with
	 * a pointer from an older generation to a younger one. 
	 */
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
        entry->next = TO_WITHOUTK(NO_PTREC);

	trec->tail->next = TO_WITHOUTK(entry); //append to end
	/*
	bdescr * desc = Bdescr((StgPtr)trec->tail);
	if(desc->gen_no > 0 && trec->tail->header.info != &stg_PTREC_WITHOUTK_MUT_info){
	    //   printf("Adding to remember set, tail is in generation %d\n", desc->gen_no);
	    cap->pastmStats.fastForwards++;
	    recordClosureMutated(cap, (StgClosure*)trec->tail);
	}
	*/

	trec->tail = entry;
        trec->capture_freq--;
    }
    return val; 
}

void ord_stmWriteTVar(Capability *cap,
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

StgPTRecWithK * ord_stmCommitTransaction(Capability *cap, StgPTRecHeader *trec) {
    unsigned long snapshot = trec->read_version;
    while (cas(&version_clock, snapshot, snapshot+1) != snapshot){ 
	StgPTRecWithK * checkpoint = ord_validate(trec, cap);
        if(checkpoint != (StgPTRecWithK *) PASTM_SUCCESS){
#ifdef STATS
	    if(checkpoint == TO_WITHK(PASTM_FAIL)){
		cap->pastmStats.commitTimeFullAborts++;
		return checkpoint;
	    }
	    cap->pastmStats.commitTimePartialAborts++;
#endif
	    return checkpoint;
        }
        snapshot = trec->read_version;
    }
    
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

    trec->read_set = TO_WITHOUTK(NO_PTREC);
    trec->lastK = TO_WITHK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->retry_stack = TO_OR_ELSE(NO_PTREC);
    trec->tail = TO_WITHOUTK(NO_PTREC);

    version_clock = snapshot + 2;//unlock clock
    return (StgPTRecWithK *)PASTM_SUCCESS;
}



void ord_printSTMStats(){
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
    printf("Heap Objects remembered = %lu\n", stats.fastForwards);
    printf("Fast Forwarded through %lu reads\n", stats.fastForwardAttempts);
#endif
}
