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
#define TO_WITHK(x) ((PTRecWithK*)x)
#define TO_WITHOUTK(x) ((PTRecWithoutK*)x)
#define TO_WRITE_SET(x) ((StgWriteSet*)x)
#define TO_CLOSURE(x) ((StgClosure*)x)
#define TO_OR_ELSE(x) ((StgPTRecOrElse *)x)
#define TO_CHUNK(x)   ((StgPTRecChunk *)x)

#define PASTM_SUCCESS              ((StgClosure*)(void*)&stg_PA_STM_SUCCESS_closure)
#define PASTM_FAIL                 ((StgClosure*)(void*)&stg_PA_STM_FAIL_closure)
#define NO_PTREC                   ((StgPTRecHeader *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER               &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER            &stg_PTREC_WITHOUTK_info
#define WRITESET_HEADER            &stg_WRITE_SET_info

#define TRACE(_x...) debugTrace(DEBUG_stm, "STM: " _x)

static volatile unsigned long version_clock = 0;

#define STATS

#ifdef STATS
static StgPASTMStats stats = {0, 0, 0, 0, 0};
#endif

//TRec 
typedef struct {
    StgHeader                  header;
    StgPTRecChunk             *read_set;
    StgPTRecChunk             *lastK;
    StgWriteSet               *write_set;
    StgPTRecOrElse            *retry_stack;
    StgPTRecChunk             *last_chunk;
    unsigned long              read_version;
    StgInt64                   capture_freq;
    StgInt                     numK;
    StgWord                    lastKOffset;
} TRec;

static StgTVar dummyTV = {.header = {.info = &stg_NO_PTREC_info /*, TODO: add profiling field*/}, 
                          .current_value = PASTM_SUCCESS, //dummy value
                          .first_watch_queue_entry = ((StgTVarWatchQueue *)(void *)&stg_END_STM_WATCH_QUEUE_closure),
                          .num_updates = 0};

StgPTRecChunk * getChunk(Capability * cap){
    StgPTRecChunk * chunks = cap->free_ptrec_chunks;
    if(chunks != TO_CHUNK(NO_PTREC)){
	cap->free_ptrec_chunks = chunks->prev_chunk;
    }else{
	chunks = (StgPTRecChunk*)allocate(cap, sizeofW(StgPTRecChunk));
	SET_HDR(chunks, &stg_PTREC_CHUNK_info, CCS_SYSTEM);
    }
    chunks->prev_chunk = TO_CHUNK(NO_PTREC);
    chunks->next_entry_idx = 0;
    return chunks;
}

TRec * ord_stmStartTransaction(Capability *cap, TRec * ptrec) {
    if(ptrec == (TRec*)NO_PTREC){
	ptrec = (TRec *)allocate(cap, sizeofW(TRec));
	SET_HDR(ptrec , &stg_CHUNKED_PTREC_HEADER_info, CCS_SYSTEM);
	cap->pastmStats.commitTimeFullAborts = 0;
	cap->pastmStats.commitTimePartialAborts = 0;
	cap->pastmStats.eagerFullAborts = 0;
	cap->pastmStats.eagerPartialAborts = 0;
    }
    
    StgPTRecChunk * c = getChunk(cap);
    ptrec->read_set = c;
    ptrec->write_set = TO_WRITE_SET(NO_PTREC);
    ptrec->lastK = TO_CHUNK(NO_PTREC);
    ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);
    ptrec->last_chunk = c;
    
    //get a read version
    ptrec->read_version = version_clock;
    while((ptrec->read_version & 1) != 0){
        ptrec->read_version = version_clock;
    }
    ptrec->capture_freq = ((unsigned long)START_FREQ << 32) + START_FREQ ; 
    ptrec->numK = 0;
    ptrec->lastKOffset = 0;

    return ptrec;
}

//TODO: recycle trec chunks
static inline void clearTRec(Capability * cap, TRec * trec){
    //leave one chunk on the read set.
    cap->free_ptrec_chunks->prev_chunk = trec->read_set->prev_chunk;
    trec->read_set->prev_chunk = TO_CHUNK(NO_PTREC);
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->retry_stack = TO_OR_ELSE(NO_PTREC);
    trec->last_chunk = trec->read_set;
    trec->lastK = TO_CHUNK(NO_PTREC);
    trec->lastKOffset = 0;
    trec->read_set->next_entry_idx = 0;

    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
    trec->capture_freq = capture_freq | (capture_freq >> 32);
    trec->numK = 0;
}


static PTRecWithK * ord_validate(TRec * trec, Capability * cap){
 RETRY:
    while(TRUE){
	unsigned long time = version_clock;
	if((time & 1) != 0){
	    continue;
	}
	trec->read_version = time;
	
	StgPTRecChunk * chunk = trec->read_set;
	StgPTRecChunk * checkpoint = NULL;
	StgInt checkpointOffset = 0;
	StgInt kCount = 0;
	
	while(chunk != TO_CHUNK(NO_PTREC)){
	    StgWord i = 0;
	    while(i < chunk->next_entry_idx){
		PTRecWithoutK * entry = (PTRecWithoutK *)&(chunk->entries[i]);
		if(entry->tvar->current_value != entry->read_value){
		    if(checkpoint != NULL){ //we have a checkpoint
			PTRecWithK * withK = (PTRecWithK*)(checkpoint->entries + checkpointOffset);
			StgClosure * val = withK->tvar->current_value;
			if(time == version_clock){
			    withK->read_value = val; //apply the continuation to this in C--
			    
			    trec->write_set = withK->write_set;
			    trec->lastK = checkpoint;
			    trec->lastKOffset = checkpointOffset;
			    trec->last_chunk = checkpoint;
			    StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
			    trec->capture_freq = capture_freq | (capture_freq >> 32);
			    trec->numK = kCount;
			    return withK;
			}else{
			    goto RETRY;
			}
		    }else{//checkpoint == NULL
			clearTRec(cap, trec);
			return TO_WITHK(PASTM_FAIL);
		    }
		}
		if(entry->size == 7){
		    checkpoint = chunk;
		    checkpointOffset = i;
		    kCount++;
		}
		i += entry->size;
	    }
	    chunk = chunk->prev_chunk;
	}
	if(time == version_clock){
	    return TO_WITHK(PASTM_SUCCESS);
	}
    }
}


/*typedef struct PTRecWithK_{
  StgWord                size;
  StgTVar               *tvar;
  StgClosure            *read_value;
  StgWriteSet           *write_set;
  StgClosure            *continuation;
  struct StgPTRecChunk_ *prev_k;         //chunk of previous checkpoint
  StgWord                prev_k_offset;  //offset into that chunk
  } PTRecWithK;*/

StgClosure * ord_stmReadTVar(Capability * cap, TRec * trec, 
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
	PTRecWithK * checkpoint = ord_validate(trec, cap);
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
        val = tvar->current_value;
    }
    
    if(trec->numK < KBOUND){//Still room for more
        if((trec->capture_freq & 0xFFFFFFFF) == 0){//Store the continuation
	    
	    StgPTRecChunk * lastChunk = trec->last_chunk;
	    if(lastChunk->next_entry_idx + 7 > PTREC_CHUNK_SIZE){
		lastChunk->prev_chunk = getChunk(cap);
		lastChunk = lastChunk->prev_chunk;
		trec->last_chunk = lastChunk;
	    }

	    PTRecWithK * entry = (PTRecWithK*)(lastChunk->entries + lastChunk->next_entry_idx);
	    entry->size = 7;
	    entry->tvar = tvar;
	    entry->read_value = val;
	    entry->write_set = trec->write_set;
	    entry->continuation = k;
	    entry->prev_k = trec->lastK;
	    entry->prev_k_offset = trec->lastKOffset;

            trec->lastK = lastChunk;
	    trec->lastKOffset = lastChunk->next_entry_idx;
            trec->numK++;
	    
	    lastChunk->next_entry_idx += 7;
	    
            if(trec->numK == KBOUND)
                trec->capture_freq <<= 1; //double the frequency
            trec->capture_freq |= (trec->capture_freq >> 32);
            
        }else{//Don't store the continuation

	    StgPTRecChunk * lastChunk = trec->last_chunk;
	    if(lastChunk->next_entry_idx + 3 > PTREC_CHUNK_SIZE){
		lastChunk->prev_chunk = getChunk(cap);
		lastChunk = lastChunk->prev_chunk;
		trec->last_chunk = lastChunk;
	    }

	    PTRecWithoutK * entry = (PTRecWithoutK*)(lastChunk->entries + lastChunk->next_entry_idx);
	    entry->size = 3;
	    entry->tvar = tvar;
	    entry->read_value = val;

	    lastChunk->next_entry_idx += 3;
            trec->capture_freq--;
        }
    }else{//filter the read set

	StgPTRecChunk * chunk = trec->lastK;
	StgWord offset = trec->lastKOffset;
	int numK = trec->numK;

	while(chunk != TO_CHUNK(NO_PTREC)){
	    PTRecWithK * withK = (PTRecWithK* )(chunk->entries + offset);
	    if(withK->prev_k != TO_CHUNK(NO_PTREC)){
		StgPTRecChunk * middleChunk = withK->prev_k;
		PTRecWithK * dropped = (PTRecWithK*)(middleChunk->entries + withK->prev_k_offset);
		withK->prev_k = dropped->prev_k;
		withK->prev_k_offset = dropped->prev_k_offset;
		dropped->continuation = TO_CLOSURE(NO_PTREC);
		numK--;
		chunk = withK->prev_k;
		offset = withK->prev_k_offset;
	    }else{
		break;
	    }
	}
	
        trec->numK = numK;

	StgPTRecChunk * lastChunk = trec->last_chunk;
	if(lastChunk->next_entry_idx + 3 > PTREC_CHUNK_SIZE){
	    lastChunk->prev_chunk = getChunk(cap);
	    lastChunk = lastChunk->prev_chunk;
	    trec->last_chunk = lastChunk;
	}
	
	PTRecWithoutK * entry = (PTRecWithoutK*)(lastChunk->entries + lastChunk->next_entry_idx);
	entry->size = 3;
	entry->tvar = tvar;
	entry->read_value = val;
	
	lastChunk->next_entry_idx += 3;
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

PTRecWithK * ord_stmCommitTransaction(Capability *cap, TRec *trec) {
    unsigned long snapshot = trec->read_version;
    while (cas(&version_clock, snapshot, snapshot+1) != snapshot){ 
	PTRecWithK * checkpoint = ord_validate(trec, cap);
        if(checkpoint != (PTRecWithK *) PASTM_SUCCESS){
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
    
    StgWriteSet * write_set = trec->write_set;
    while(write_set != TO_WRITE_SET(NO_PTREC)){
        StgTVar * tvar = write_set->tvar;
        tvar->current_value = write_set->val;
        dirty_TVAR(cap,tvar);
        write_set = write_set->next;
    }
    
#ifdef STATS
    //Since version clock is locked, these don't need to be atomic
    stats.commitTimeFullAborts += cap->pastmStats.commitTimeFullAborts;
    stats.commitTimePartialAborts += cap->pastmStats.commitTimePartialAborts;
    stats.eagerFullAborts += cap->pastmStats.eagerFullAborts;
    stats.eagerPartialAborts += cap->pastmStats.eagerPartialAborts;
    stats.numCommits++;
    cap->pastmStats.commitTimeFullAborts = 0;
    cap->pastmStats.commitTimePartialAborts = 0;
    cap->pastmStats.eagerFullAborts = 0;
    cap->pastmStats.eagerPartialAborts = 0;
#endif
    version_clock = snapshot + 2;//unlock clock

    clearTRec(cap, trec);

    return (PTRecWithK *)PASTM_SUCCESS;
}



void ord_printSTMStats(){
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
