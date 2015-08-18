#include "Rts.h"
#include "Trace.h"
#include "rts/Threads.h"
#include "sm/Storage.h"
#include <stdio.h>
#include "rts/EventLogFormat.h"
#include "eventlog/EventLog.h"

#if defined(HAVE_MACH_ABSOLUTE_TIME)
#  include <mach/mach_time.h>
#elif defined(HAVE_CLOCK_GETTIME)
#  include <time.h>
#else
#  include <time.h>
#  include <sys/time.h>
#endif

#define US_PER_NANOSECOND     1000
#define MS_PER_SECOND  	      1000
#define US_PER_SECOND	      (1000 * MS_PER_SECOND)
#define NS_PER_SECOND	      (1000 * US_PER_SECOND)

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
#define TimeToSecondsDbl(t) ((double)(t) / TIME_RESOLUTION)

static volatile unsigned long version_clock = 0;

//use -optc-DSTATS to enable statistics

StgPTRecHeader * pa_stmStartTransaction(Capability *cap, StgPTRecHeader * ptrec) {
#ifdef TRACING  
    postEvent (cap, EVENT_START_TX);
#endif

    if(ptrec == NO_PTREC){
	ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
    	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
    	ptrec->tail = TO_WITHOUTK(NO_PTREC);
    	ptrec->read_set = TO_WITHOUTK(NO_PTREC);
    	ptrec->lastK = TO_WITHK(NO_PTREC);
    	ptrec->write_set = TO_WRITE_SET(NO_PTREC);
    	ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);
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

StgPTRecHeader * pa_stmStartTransactionWithEvent(Capability *cap, StgPTRecHeader * ptrec, StgWord event) {
#ifdef TRACING  
    postStartTX(cap, ((StgWord*)(event & (~7)))[1]);
#endif

    if(ptrec == NO_PTREC){
	ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
    	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
    	ptrec->tail = TO_WITHOUTK(NO_PTREC);
    	ptrec->read_set = TO_WITHOUTK(NO_PTREC);
    	ptrec->lastK = TO_WITHK(NO_PTREC);
    	ptrec->write_set = TO_WRITE_SET(NO_PTREC);
    	ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);
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

StgPTRecHeader * pa_stmStartTransactionAfterAbort(Capability *cap, StgPTRecHeader * ptrec) {
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
    trec->read_set = TO_WITHOUTK(NO_PTREC);
    trec->lastK = TO_WITHK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->retry_stack = TO_OR_ELSE(NO_PTREC);
    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
    trec->capture_freq = capture_freq | (capture_freq >> 32);
    trec->numK = 0;
}

#ifdef ABORT
static StgBool shouldAbort = TRUE;
#endif

static StgPTRecWithK * pa_validate(StgPTRecHeader * trec, Capability * cap){
#ifdef TRACING
    postEvent (cap, EVENT_BEGIN_COMMIT);
#endif
    while(TRUE){
        unsigned long time = version_clock;
        if((time & 1) != 0){
            continue; //clock is locked
        }
        trec->read_version = time;

#ifdef ABORT
	if(shouldAbort){
	    StgPTRecWithK * ptr = trec->lastK;
	    int i;
	    int n = trec->numK / 2;
	    for(i = 0; i < n; i++){
		ptr = ptr->prev_k;
		trec->numK--;
	    }
	    trec->read_set = TO_WITHOUTK(ptr);
	    trec->write_set = ptr->write_set;
	    trec->lastK = ptr;
	    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
	    trec->capture_freq = capture_freq | (capture_freq >> 32);

	    shouldAbort = FALSE;

	    return ptr;
	}
	shouldAbort = TRUE;

#endif
        //validate read set
        StgPTRecWithoutK * ptr = trec->read_set;
        StgPTRecWithK *checkpoint = TO_WITHK(PASTM_SUCCESS);
        StgInt kCount = 0;
	StgInt i = 0;

	/*
	//this loop assumes that we do not need a checkpoint
    NOCHKPNT:
        while(ptr != TO_WITHOUTK(NO_PTREC)){
	    if(ptr->read_value != ptr->tvar->current_value){
		if(ptr->header.info == WITHK_HEADER){
		    checkpoint = TO_WITHK(ptr);
		}else{
		    checkpoint = TO_WITHK(PASTM_FAIL);
		    ptr = ptr->next;
		    goto CHKPNT;  //look for a checkpoint
		}
	    }
	    ptr = ptr->next;
	}

    CHKPNT:
	while(ptr != TO_WITHOUTK(NO_PTREC)){
	    if(ptr->read_value != ptr->tvar->current_value){
		if(ptr->header.info == WITHK_HEADER){
		    checkpoint = TO_WITHK(ptr);
		    ptr = ptr->next;
		    goto NOCHKPNT;
		}
	    }else if(ptr->header.info == WITHK_HEADER){  //valid and checkpointed
		checkpoint = TO_WITHK(ptr);
		ptr = ptr->next;
		goto NOCHKPNT;
	    }
	    ptr = ptr->next;
	}
	*/
	while(ptr != TO_WITHOUTK(NO_PTREC)){
	    if(ptr->header.info == WITHOUTK_HEADER){
		if(ptr->read_value != ptr->tvar->current_value){
		    checkpoint = TO_WITHK(PASTM_FAIL);
		    kCount = 0;
		    i = 0;
		}
	    }else{
		StgPTRecWithK * withK = TO_WITHK(ptr);
		if(withK->read_value != withK->tvar->current_value){
		    checkpoint = withK;
		    ptr = ptr->next;
		    kCount = 1;
		    i = 1;
		    continue;
		}else if(checkpoint == TO_WITHK(PASTM_FAIL)){ //Valid and we need a checkpoint
		    checkpoint = withK;
		}
		kCount++;
	    }
            ptr = ptr->next;
	    i++;
        }


        if(checkpoint == TO_WITHK(PASTM_FAIL)){ //no checkpoint found, but we need to abort
	    clearTRec(trec);
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

#ifdef STATS
		cap->pastmStats.fastForwardAttempts += i;
#endif

                StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
                trec->capture_freq = capture_freq | (capture_freq >> 32);
                trec->numK = kCount;
                return checkpoint;
            }else{
                continue; //revalidate the whole log
            }
            
        }

        if(time == version_clock){
            return checkpoint; //necessarily PASTM_SUCCESS
        }
        //Validation succeeded, but someone else committed in the meantime, loop back around...
    }
}

STATIC_INLINE uint64_t TIMER_Now ()
{
#if defined(HAVE_MACH_ABSOLUTE_TIME)
    return mach_absolute_time();
#elif defined(HAVE_CLOCK_GETTIME)
    struct timespec t;
    clock_gettime (CLOCK_REALTIME, &t);
    return (NS_PER_SECOND * (uint64_t)t.tv_sec) + (uint64_t)t.tv_nsec;
#else
    struct timeval t;
    gettimeofday (&t, 0);
    return (NS_PER_SECOND * (uint64_t)t.tv_sec) + (US_PER_NANOSECOND * (uint64_t)t.tv_usec);
#endif
}

StgClosure * pa_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
			    StgTVar * tvar, StgClosure * k){
#ifdef STMPROF
    uint64_t startRead = TIMER_Now();
#endif
    
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
#ifdef STATS
	    if(checkpoint == TO_WITHK(PASTM_FAIL)){
		cap->pastmStats.eagerFullAborts++;
#ifdef TRACING  
		postEvent (cap, EVENT_EAGER_FULL_ABORT);
#endif
		return TO_CLOSURE(checkpoint);
	    }
#ifdef TRACING  
	    postEvent (cap, EVENT_EAGER_PARTIAL_ABORT);
#endif
	    cap->pastmStats.eagerPartialAborts++;
#endif
	    return TO_CLOSURE(checkpoint);
        }
#ifdef STATS
	cap->pastmStats.tsExtensions++;
#endif
	val = tvar->current_value;
    }
    
    /* 
    if((trec->capture_freq & 0xFFFFFFFF) == 0){//Store the continuation
	printf("Storing continuation\n");
	StgPTRecWithK * entry = (StgPTRecWithK *)allocate(cap, sizeofW(StgPTRecWithK));
	SET_HDR(entry , &stg_PTREC_WITHK_info, CCS_SYSTEM);
	entry->tvar = tvar;
	entry->read_value = val;
	entry->next = trec->read_set;
	entry->write_set = trec->write_set;
	entry->continuation = k;
	entry->prev_k = trec->lastK;
	trec->read_set = TO_WITHOUTK(entry);
	trec->lastK = entry;

	if(trec->numK == KBOUND-1){
	    int numK = trec->numK;
	    while(entry != TO_WITHK(NO_PTREC)){
		if(entry->prev_k != TO_WITHK(NO_PTREC)){
		    StgPTRecWithK * dropped = entry->prev_k;
		    dropped->continuation = TO_CLOSURE(NO_PTREC);
		    entry->prev_k = entry->prev_k->prev_k;
		    entry = entry->prev_k;
		    SET_HDR(dropped, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
		    numK--;
		}else{
		    break;
		}
	    }
	    trec->numK = numK + 1;
	    trec->capture_freq <<= 1;
	    trec->capture_freq |= (trec->capture_freq >> 32);
	}else{
	    trec->numK++;
	}
    }else{//Don't store the continuation
	StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
	SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
	entry->tvar = tvar;
	entry->read_value = val;
	entry->next = trec->read_set;
	trec->read_set = entry;
	trec->capture_freq--;
    }
    */
    
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
#ifdef STMPROF
    cap->profileSTM.readTime += TIMER_Now() - startRead;
#endif
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

#ifdef ABORT
    StgPTRecWithK * checkpoint = pa_validate(trec, cap);
    if(checkpoint != (StgPTRecWithK *) PASTM_SUCCESS){
	return checkpoint;
    }
#endif

    unsigned long snapshot = trec->read_version;
    while (cas(&version_clock, snapshot, snapshot+1) != snapshot){ 
	StgPTRecWithK * checkpoint = pa_validate(trec, cap);
        if(checkpoint != (StgPTRecWithK *) PASTM_SUCCESS){
#ifdef STATS
	    if(checkpoint == TO_WITHK(PASTM_FAIL)){
		cap->pastmStats.commitTimeFullAborts++;
#ifdef TRACING  
		postEvent (cap, EVENT_COMMIT_FULL_ABORT);
#endif
		return checkpoint;
	    }
	    cap->pastmStats.commitTimePartialAborts++;
#endif
#ifdef TRACING  
	    postEvent (cap, EVENT_COMMIT_PARTIAL_ABORT);
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

    //Clear the trec, so it can be GC'd
    trec->read_set = TO_WITHOUTK(NO_PTREC);
    trec->lastK = TO_WITHK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->retry_stack = TO_OR_ELSE(NO_PTREC);
    trec->tail = TO_WITHOUTK(NO_PTREC);
    
    version_clock = snapshot + 2;//unlock clock

    postEvent (cap, EVENT_COMMIT_TX);

    return (StgPTRecWithK *)PASTM_SUCCESS;
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
    printf("Fast Forwarded through %lu reads\n", stats.fastForwardAttempts);
#endif
}

/*
 * Retry the transaction, we check to see that we 
 * have something on our retry stack before entering
 * this function.  This will return the alternative
 * branch of the orElse and set the read set/write set
 * appropriately.  
 */
StgClosure * pa_stmRetry(StgPTRecHeader * trec){
    trec->write_set = trec->retry_stack->write_set;
    StgClosure * alt = trec->retry_stack->alt;
    trec->retry_stack = trec->retry_stack->next;
    return alt;
}

//I think the header can be clean, since it isn't going to be pointing
//to anything in a younger generation
static StgTVar retryTV = {.header = {.info = &stg_TVAR_CLEAN_info /*, TODO: add profiling field*/}, 
                          .current_value = PASTM_SUCCESS, //dummy value
                          .first_watch_queue_entry = ((StgTVarWatchQueue *)(void *)&stg_END_STM_WATCH_QUEUE_closure),
                          .num_updates = 0};

void pa_stmCatchRetry(Capability *cap, StgPTRecHeader * trec, 
		      StgClosure * alt){
    StgPTRecOrElse * orelse = (StgPTRecOrElse*)allocate(cap, sizeofW(StgPTRecOrElse));
    SET_HDR(orelse, &stg_PTREC_OR_ELSE_info, CCS_SYSTEM);
    orelse->alt = alt;
    orelse->write_set = trec->write_set;
    orelse->next = trec->retry_stack;
    trec->retry_stack = orelse;
}
