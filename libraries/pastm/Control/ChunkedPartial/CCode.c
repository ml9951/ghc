#include "Rts.h"
#include "Trace.h"
#include "rts/Threads.h"
#include "sm/Storage.h"
#include <stdio.h>

#define TRUE 1
#define FALSE 0
#define KBOUND 20
#define START_FREQ 4

//TRec 
typedef struct {
    StgHeader                  header;        //heap object header
    StgPTRecChunk             *read_set;      //front of the read set (first chunk)
    StgPTRecWithK             *lastK;         //DO NOT USE: contains the original STM code for aborts
    StgPTRecChunk             *tail;          //last chunk of linked list
    StgWriteSet               *write_set;     //write set
    StgPTRecOrElse            *retry_stack;   //stack of retry entries.  Currently unused
    unsigned long              read_version;  //time the transaction started
    StgInt64                   abort_ret_val; //if partially aborting, apply this to the continuation
    StgInt                     numK;          //not used
} TRec;

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
#define NO_PTREC                   ((TRec *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER               &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER            &stg_PTREC_WITHOUTK_info
#define WRITESET_HEADER            &stg_WRITE_SET_info

static volatile unsigned long version_clock = 0;

StgPTRecChunk * alloc_chunk(Capability * cap){
    StgPTRecChunk * chunk = (StgPTRecChunk *)allocate(cap, sizeofW(StgPTRecChunk));
    SET_HDR(chunk, &stg_PTREC_CHUNK_info, CCS_SYSTEM);
    chunk->write_set = TO_WRITE_SET(NO_PTREC);
    chunk->checkpoint = TO_CLOSURE(NO_PTREC);
    chunk->prev_chunk = (StgPTRecChunk*)NO_PTREC;
    chunk->next_entry_idx = 0;
    return chunk;
}

TRec * ptl2_stmStartTransaction(Capability *cap, TRec * ptrec){
    if(ptrec == NO_PTREC){
	ptrec = (TRec *)allocate(cap, sizeofW(StgPTRecHeader));
	SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
	ptrec->lastK = TO_WITHK(NO_PTREC);
    }

    StgPTRecChunk * chunk = alloc_chunk(cap);
    ptrec->read_set = chunk;
    ptrec->tail = chunk;
    ptrec->write_set = TO_WRITE_SET(NO_PTREC);
    ptrec->retry_stack = TO_OR_ELSE(NO_PTREC);
    
    ptrec->read_version = version_clock;
    ptrec->abort_ret_val = 0; 
    ptrec->numK = 0;

    return ptrec;
}

static void clearTRec(TRec * trec){
    trec->read_set = trec->tail;
    trec->tail->next_entry_idx = 0;
    trec->write_set = TO_WRITE_SET(NO_PTREC);
    trec->retry_stack = TO_OR_ELSE(NO_PTREC);
    trec->read_version = version_clock;
}

void releaseLocks(StgWriteSet * ws, StgWriteSet * sentinel){
    while(ws != sentinel){
	StgTL2TVar * tvar = TO_TL2(ws->tvar);
	tvar->currentStamp = tvar->oldStamp;
	ws = ws->next;
    }
}

static StgPTRecChunk * force_abort(TRec * trec, Capability * cap){
    unsigned long newStamp = atomic_inc(&(version_clock), 2);
    unsigned long myStamp = trec->read_version;
    StgPTRecChunk * ptr = trec->read_set;
    StgPTRecChunk * checkpoint = (StgPTRecChunk*)PASTM_FAIL;
    StgInt kCount = 0;

 RETRY:
    while(ptr != (StgPTRecChunk*)NO_PTREC){
	int i;
	for(i = 0; i < ptr->next_entry_idx; i++){
	    StgTL2TVar * tvar = ptr->entries[i];
	    unsigned long s = tvar->currentStamp;
	    if(s > myStamp || LOCKED(s)){
		if(checkpoint != (StgPTRecChunk*)PASTM_FAIL){
		    //re-read
		    StgTL2TVar * tvar = checkpoint->entries[PTREC_CHUNK_SIZE - 1];
		    StgClosure * val; 
		    unsigned long stamp1, stamp2;
		    
		    stamp1 = tvar->currentStamp;
		    val = tvar->current_value;
		    stamp2 = tvar->currentStamp;
		    
		    if(LOCKED(stamp1) || stamp1 != stamp2 || stamp1 > newStamp){
			//try validating again, but don't go past this checkpoint
			myStamp = newStamp;
			newStamp = version_clock;
			checkpoint->prev_chunk = (StgPTRecChunk*)NO_PTREC;
			checkpoint = (StgPTRecChunk*)PASTM_FAIL;
			kCount = 0;
			goto RETRY;
		    }
		    trec->read_version = newStamp;
		    trec->tail = checkpoint;
		    trec->tail->next_entry_idx = 0;
		    trec->tail->write_set = TO_WRITE_SET(NO_PTREC);
		    trec->tail->checkpoint = TO_CLOSURE(NO_PTREC);
		    trec->abort_ret_val = (StgInt64)val;
		    return checkpoint;
		}
		clearTRec(trec);
		return checkpoint;
	    }
	}
	if(i == PTREC_CHUNK_SIZE){
	    checkpoint = ptr;
	}
	ptr = ptr->prev_chunk;
    }
    
    //re-read
    StgTL2TVar * tvar = checkpoint->entries[PTREC_CHUNK_SIZE - 1];
    StgClosure * val; 
    unsigned long stamp1, stamp2;
    
    stamp1 = tvar->currentStamp;
    val = tvar->current_value;
    stamp2 = tvar->currentStamp;
    
    if(LOCKED(stamp1) || stamp1 != stamp2 || stamp1 > newStamp){
	//try validating again, but don't go past this checkpoint
	myStamp = newStamp;
	newStamp = version_clock;
	checkpoint->prev_chunk = (StgPTRecChunk*)NO_PTREC;
	checkpoint = (StgPTRecChunk*)PASTM_FAIL;
	kCount = 0;
	goto RETRY;
    }
    trec->read_version = newStamp;
    trec->tail = checkpoint;
    trec->tail->next_entry_idx = 0;
    trec->tail->write_set = TO_WRITE_SET(NO_PTREC);
    trec->tail->checkpoint = TO_CLOSURE(NO_PTREC);
    trec->abort_ret_val = (StgInt64)val;
    return checkpoint;
}
static StgPTRecChunk * validate(TRec * trec, Capability * cap){
    unsigned long newStamp = atomic_inc(&(version_clock), 2);
    unsigned long myStamp = trec->read_version;
    StgPTRecChunk * ptr = trec->read_set;
    StgPTRecChunk * checkpoint = (StgPTRecChunk*)PASTM_FAIL;
    StgInt kCount = 0;

    while(ptr != (StgPTRecChunk*)NO_PTREC){

	int i;
	for(i = 0; i < ptr->next_entry_idx; i++){
	    StgTL2TVar * tvar = ptr->entries[i];
	    unsigned long s = tvar->currentStamp;
	    if(s > myStamp || LOCKED(s)){
		if(checkpoint != (StgPTRecChunk*)PASTM_FAIL){
		    //re-read
		    StgTL2TVar * tvar = checkpoint->entries[PTREC_CHUNK_SIZE - 1];
		    StgClosure * val; 
		    unsigned long stamp1, stamp2;
		    
		    stamp1 = tvar->currentStamp;
		    val = tvar->current_value;
		    stamp2 = tvar->currentStamp;
		    
		    if(LOCKED(stamp1) || stamp1 != stamp2 || stamp1 > newStamp){
			checkpoint->prev_chunk = (StgPTRecChunk*)NO_PTREC;
			return force_abort(trec, cap);
		    }
		    trec->read_version = newStamp;
		    trec->tail = checkpoint;
		    trec->tail->next_entry_idx = 0;
		    trec->tail->write_set = TO_WRITE_SET(NO_PTREC);
		    trec->tail->checkpoint = TO_CLOSURE(NO_PTREC);
		    trec->abort_ret_val = (StgInt64)val;
		    return checkpoint;
		}
		clearTRec(trec);
		return checkpoint;
	    }
	}
	if(i == PTREC_CHUNK_SIZE){
	    checkpoint = ptr;
	}
	ptr = ptr->prev_chunk;
    }
    
    trec->read_version = newStamp;
    return (StgPTRecChunk*)PASTM_SUCCESS;
}

int i = 0;
StgBool flag = FALSE;

StgClosure * ptl2_stmReadTVar(Capability * cap, TRec * trec, 
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
	StgPTRecChunk * res = validate(trec, cap);
	if(res != (StgPTRecChunk*)PASTM_SUCCESS){
	    return TO_CLOSURE(res);
	}
	//timestamp was extended, try reading again
	goto retry;
    }

    StgPTRecChunk * current_chunk = trec->tail;
    current_chunk->entries[current_chunk->next_entry_idx] = tvar;
    if(current_chunk->next_entry_idx == PTREC_CHUNK_SIZE-1){
	current_chunk->write_set = trec->write_set;
	current_chunk->checkpoint = k;
	StgPTRecChunk * new_chunk = alloc_chunk(cap);
	current_chunk->prev_chunk = new_chunk;
	trec->tail = new_chunk;
	new_chunk->prev_chunk = (StgPTRecChunk *)NO_PTREC;
    }else{
	current_chunk->next_entry_idx++;
    }
    return val; 
}

void ptl2_stmWriteTVar(Capability *cap,
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

StgPTRecChunk * ptl2_stmCommitTransaction(Capability *cap, TRec *trec, StgThreadID id) {    
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
	    return (StgPTRecChunk*)PASTM_FAIL;
	}

	tvar->oldStamp = stamp;
	trailer = &(ws_ptr->next);
	ws_ptr = ws_ptr->next;
    }

    unsigned long write_version = atomic_inc(&version_clock, 2);
    
    //validate read set
    StgPTRecChunk * checkpoint = NULL;
    StgPTRecChunk * ptr = trec->read_set;
    while(ptr != (StgPTRecChunk*)NO_PTREC){
	int i;
	for(i = 0; i < ptr->next_entry_idx; i++){
	    StgTL2TVar * tvar = ptr->entries[i];
	    unsigned long s = tvar->currentStamp;
	    if(s != lockVal && (s > myStamp || LOCKED(s))){
		releaseLocks(trec->write_set, TO_WRITE_SET(NO_PTREC));
		if(checkpoint != NULL){
		    //re-read
		    StgTL2TVar * tvar = checkpoint->entries[PTREC_CHUNK_SIZE - 1];
		    StgClosure * val; 
		    unsigned long stamp1, stamp2;
			
		    stamp1 = tvar->currentStamp;
		    val = tvar->current_value;
		    stamp2 = tvar->currentStamp;
		    
		    if(LOCKED(stamp1) || stamp1 != stamp2 || stamp1 > write_version){
			//try validating again, but don't go past this checkpoint
			checkpoint->prev_chunk = (StgPTRecChunk*)NO_PTREC;
			return force_abort(trec, cap);
		    }
		    trec->read_version = write_version;
		    trec->tail = checkpoint;
		    trec->abort_ret_val = (StgInt64)val;
		    return checkpoint;
		}
		clearTRec(trec);
		return (StgPTRecChunk*)PASTM_FAIL;
	    }
	}
	if(i == PTREC_CHUNK_SIZE){
	    checkpoint = ptr;
	}
	ptr = ptr->prev_chunk;
    }
    
    //push write set into global store
    ws_ptr = trec->write_set;
    while(ws_ptr != TO_WRITE_SET(NO_PTREC)){
	StgTL2TVar * tvar = TO_TL2(ws_ptr->tvar);
	tvar->current_value = ws_ptr->val;
	tvar->currentStamp = write_version;
	dirty_TL2_TVAR(cap, tvar);
	ws_ptr = ws_ptr->next;
    }
    return (StgPTRecChunk*)PASTM_SUCCESS;
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
