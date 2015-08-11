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

typedef struct{
    StgHeader                  header;
    StgPTRecWithoutK          *read_set;
    StgPTRecWithK             *lastK;
    StgPTRecWithoutK          *tail; //last element of linked list
    StgWriteSet               *write_set;
    StgPTRecWithK             *ff_read_set;
    unsigned long              read_version;
    StgInt64                   capture_freq;
    StgInt                     numK;
}TRec;

StgBool polyEq(StgClosure * p1, StgClosure * p2){
    p1 = UNTAG_CLOSURE(p1);
    p2 = UNTAG_CLOSURE(p2);
    if(p1->header.info != p2->header.info){
	return FALSE;
    }
    
    StgInfoTable * itbl = get_itbl(p1);

    int nptrs = (int) itbl->layout.payload.nptrs;
    int ptrs = (int) itbl->layout.payload.ptrs;

    int i;

    for(i = 0; i < ptrs; i++){
	//short circuit if they are pointer equal, otherwise recurse
	if(p1->payload[i] != p2->payload[i] && !polyEq(p1->payload[i], p2->payload[i])){
	    return FALSE;
	}
    }

    for(i = ptrs; i < nptrs + ptrs; i++){
	if(p1->payload[i] != p2->payload[i]){
	    return FALSE;
	}
    }
    
    return TRUE;
}

void sanity_check(TRec * trec){
    StgPTRecWithK * ptr = trec->ff_read_set;
    while(ptr != TO_WITHK(NO_PTREC)){
	StgInt updates = ptr->tvar->num_updates;
	if(updates <= 0){
	    printf("Error: tvar on ff short path has %ld updates\n", updates);
	}else{
	    printf("TVar has nonzero count of %ld\n", updates);
	}
	ptr = ptr->prev_k;
    }
}

TRec * ord_stmStartTransaction(Capability *cap, TRec * ptrec) {
    if(ptrec == (TRec*)NO_PTREC){
	ptrec = (TRec *)allocate(cap, sizeofW(StgPTRecHeader));
	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);

	StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
	SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
	
	entry->tvar = &dummyTV;
	entry->read_value = PASTM_SUCCESS;
	entry->next = TO_WITHOUTK(NO_PTREC);
	
	ptrec->read_set = entry;
	ptrec->lastK = TO_WITHK(NO_PTREC);
	ptrec->write_set = TO_WRITE_SET(NO_PTREC);
	ptrec->ff_read_set = TO_WITHK(NO_PTREC);
	ptrec->tail = entry;
    }
    
    //get a read version
    ptrec->read_version = version_clock;
    while((ptrec->read_version & 1) != 0){
        ptrec->read_version = version_clock;
    }
    ptrec->capture_freq = ((unsigned long)START_FREQ << 32) + START_FREQ ; 
    ptrec->numK = 0;

    return ptrec;
}

static inline void adjustCounts(StgPTRecWithK * incs, StgPTRecWithK * decs, TRec * trec, const char * context){
    while(incs != TO_WITHK(NO_PTREC)){
	atomic_inc((StgVolatilePtr)&(incs->tvar->num_updates), 1);
	incs = incs->prev_k;
    }
    while(decs != TO_WITHK(NO_PTREC)){
	atomic_inc((StgVolatilePtr)&(decs->tvar->num_updates), -1);
	decs = decs->prev_k;
    }
}

static inline void clearTRec(TRec * trec){
    StgPTRecWithK *old_ff = trec->ff_read_set;
    trec->ff_read_set = trec->lastK;

    adjustCounts(trec->lastK, old_ff, trec, "clearTRec");

    StgPTRecWithoutK * first = trec->read_set;
    first->next = TO_WITHOUTK(NO_PTREC);
    trec->lastK = TO_WITHK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->tail = first;
    
    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
    trec->capture_freq = capture_freq | (capture_freq >> 32);
    trec->numK = 0;
}

static StgPTRecWithK * ord_validate(TRec * trec, Capability * cap){
    
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
	
	while(ptr != TO_WITHOUTK(NO_PTREC)){
	    if(ptr->read_value != ptr->tvar->current_value){
		if(checkpoint != NULL){ //we have a checkpoint
		    //try reading from this tvar
		    StgClosure * val = checkpoint->tvar->current_value;
		    if(time == version_clock){
			checkpoint->read_value = val; //apply the continuation to this in C--
			checkpoint->next = TO_WITHOUTK(NO_PTREC);
			
			//seperate the short paths and increment counts
			StgPTRecWithK * ptr = trec->lastK;
			if(ptr != checkpoint){
			    while(ptr->prev_k != checkpoint){
				atomic_inc((StgVolatilePtr)&(ptr->tvar->num_updates), 1);
				ptr = ptr->prev_k;
			    }
			    ptr->prev_k = TO_WITHK(NO_PTREC);

			    adjustCounts(TO_WITHK(NO_PTREC), trec->ff_read_set, trec, "partial abort");

			    trec->ff_read_set = trec->lastK;
			}

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

StgPTRecWithK * fast_forward(Capability * cap, TRec * trec, StgTVar * tvar, StgWriteSet * ws, StgClosure * k){
#ifdef STATS
    cap->pastmStats.fastForwardAttempts++;
#endif
    StgPTRecWithK * shortPath = trec->ff_read_set;
    while(shortPath != TO_WITHK(NO_PTREC)){
	if(shortPath->tvar == tvar){
	    if(ws == shortPath->write_set && polyEq(k, shortPath->continuation)){

		//IMPORTANT: decrement counts before hooking read sets together, otherwise we will decrement
		//too the short path of our current read set!
		StgPTRecWithK * ff_read_set = trec->ff_read_set;
		trec->ff_read_set = TO_WITHK(NO_PTREC);
		adjustCounts(TO_WITHK(NO_PTREC), trec->ff_read_set, trec, "fast forward");

		trec->tail->next = TO_WITHOUTK(shortPath);
		recordClosureMutated(cap, (StgClosure*)trec->tail);
		shortPath->prev_k = trec->lastK;
		recordClosureMutated(cap, (StgClosure*)shortPath);
		
		int kCount = 0;
		//validate forwards
		StgPTRecWithK * checkpoint = shortPath;
		StgPTRecWithoutK * ptr = TO_WITHOUTK(shortPath);
		while(ptr != TO_WITHOUTK(ff_read_set)){ //no point in going past the last checkpoint
		    if(ptr->tvar->current_value != ptr->read_value){
			goto FINISH;
		    }
		    if(ptr->header.info == WITHK_HEADER){
			checkpoint = TO_WITHK(ptr);
			kCount++;
		    }
		    ptr = ptr->next;
		}
		checkpoint = (TO_WITHK(ptr));
		
	    FINISH:
		trec->tail = TO_WITHOUTK(checkpoint);
		trec->lastK = checkpoint;
		trec->write_set = checkpoint->write_set;
		trec->numK += kCount;
		
#ifdef STATS
		cap->pastmStats.fastForwards++;
#endif
		if(checkpoint == TO_WITHK(NO_PTREC)){
		    printf("About to return no ptrec\n");
		}

		return checkpoint;
		

	    }
	}
	shortPath = shortPath->prev_k;
    }
    return TO_WITHK(PASTM_FAIL);
}

StgClosure * ord_stmReadTVar(Capability * cap, TRec * trec, 
			     StgTVar * tvar, StgClosure * k){
    StgWriteSet * ws = trec->write_set;

    while(ws != TO_WRITE_SET(NO_PTREC)){
        if(ws->tvar == tvar){
            return ws->val;
        }
        ws = ws->next;
    }
    
    if(trec->ff_read_set != TO_WITHK(NO_PTREC) && tvar->num_updates > 0){
	StgPTRecWithK * res = fast_forward(cap, trec, tvar, trec->write_set, k);
	if(res != TO_WITHK(PASTM_FAIL)){
	    return TO_CLOSURE(res);
	}
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
	    bdescr * desc = Bdescr((StgPtr)trec->tail);
	    if(desc->gen_no > 0){
		recordClosureMutated(cap, (StgClosure*)trec->tail);
	    }

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
	    bdescr * desc = Bdescr((StgPtr)trec->tail);
	    if(desc->gen_no > 0){
		recordClosureMutated(cap, (StgClosure*)trec->tail);
	    }

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
		recordClosureMutated(cap, (StgClosure*)ptr);
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
	bdescr * desc = Bdescr((StgPtr)trec->tail);
	if(desc->gen_no > 0){
	    recordClosureMutated(cap, (StgClosure*)trec->tail);
	}
	
	trec->tail = entry;
        trec->capture_freq--;
    }
    return val; 
}

void ord_stmWriteTVar(Capability *cap,
		      TRec *trec,
		      StgTVar *tvar,
		      StgClosure *new_value) {
    StgWriteSet * newEntry = (StgWriteSet *) allocate(cap, sizeofW(StgWriteSet));
    SET_HDR(newEntry , &stg_WRITE_SET_info, CCS_SYSTEM);
    newEntry->tvar = tvar;
    newEntry->val = new_value;
    newEntry->next = trec->write_set;
    trec->write_set = newEntry;
}

StgPTRecWithK * ord_stmCommitTransaction(Capability *cap, TRec *trec) {
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
    
    adjustCounts(TO_WITHK(NO_PTREC), trec->ff_read_set, trec, "Committing transaction");
    
    StgPTRecWithoutK * first = trec->read_set;
    first->next = TO_WITHOUTK(NO_PTREC);
    trec->lastK = TO_WITHK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->ff_read_set = TO_WITHK(NO_PTREC);
    trec->tail = first;


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
    printf("Fast Forwarding attempts = %lu\n", stats.fastForwardAttempts);
    printf("Successful Fast Forwards = %lu\n", stats.fastForwards);
    printf("Total Aborts = %lu\n", stats.commitTimeFullAborts + stats.commitTimePartialAborts + 
	   stats.eagerPartialAborts + stats.eagerFullAborts);
    printf("Number of Commits = %lu\n", stats.numCommits);
#endif
}
