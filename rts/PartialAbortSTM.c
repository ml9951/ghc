/* -----------------------------------------------------------------------------
 * (c) The GHC Team 2015
 *
 * Partial Abort STM implementation.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Schedule.h"
#include "PartialAbortSTM.h"
#include "Trace.h"
#include "Threads.h"
#include "sm/Storage.h"

#include <stdio.h>

#define TRUE 1
#define FALSE 0
#define TRACE(_x...) debugTrace(DEBUG_stm, "STM: " _x) //supply "+RTS -Dm" for STM trace output 
#define KBOUND 20
#define START_FREQ 4

//casts
#define TO_WITHK(x) ((StgPTRecWithK*)x)
#define TO_WITHOUTK(x) ((StgPTRecWithoutK*)x)
#define TO_WRITE_SET(x) ((StgWriteSet*)x)
#define TO_CLOSURE(x) ((StgClosure*)x)
#define TO_OR_ELSE(x) ((StgPTRecOrElse *)x)

static volatile unsigned long version_clock = 0;

StgPTRecHeader * p_stmStartTransaction(Capability *cap) {
    TRACE("p_stmStartTransaction");
    
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

static StgPTRecWithK * validate(StgPTRecHeader * trec){
    while(TRUE){
        unsigned long time = version_clock;
        if((time & 1) != 0){
            continue; //clock is locked
        }
        TRACE("Validating: sampled version clock is currently %lu (must be even at this point)\n", time);
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
            TRACE("Validation failed an no checkpoint was found...\n");
            return checkpoint;
        }
        if(checkpoint != TO_WITHK(PASTM_SUCCESS)){ //validation failed, but we found a checkpoint
            //try reading from this tvar
            StgTVar * tvar = checkpoint->tvar;
            StgClosure * val = tvar->current_value;
            if(time == version_clock){
                TRACE("Validation failed, a checkpoint was found, and we successfully read from the checkpointed tvar\n");
                checkpoint->read_value = val; //apply the continuation to this in C--
                trec->read_set = TO_WITHOUTK(checkpoint);
                trec->write_set = checkpoint->write_set;
                trec->lastK = checkpoint;
                StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
                trec->capture_freq = capture_freq | (capture_freq >> 32);
                trec->numK = kCount;
                return checkpoint;
            }else{
                TRACE("Someone else committed while we tried to re-read from the checkpointed tvar\n");
                continue; //revalidate the whole log
            }
            
        }

        if(time == version_clock){
            TRACE("Validation succeeded\n");
            return checkpoint; //necessarily PASTM_SUCCESS
        }
        //Validation succeeded, but someone else committed in the meantime, loop back around...
    }
}

StgClosure * p_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
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
        StgPTRecWithK * checkpoint = validate(trec);
        if(checkpoint != TO_WITHK(PASTM_SUCCESS)){
            return TO_CLOSURE(checkpoint);
        }
        val = tvar->current_value;
    }
    
    if(trec->numK < KBOUND){//Still room for more
        if((trec->capture_freq & 0xFFFFFFFF) == 0){//Store the continuation
            //TRACE("Read from tvar %p, capturing a continuation...");
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
            //TRACE("Read from tvar %p, not capturing a continuation");
            StgPTRecWithoutK * entry = (StgPTRecWithoutK*)allocate(cap, sizeofW(StgPTRecWithoutK));
            SET_HDR(entry, &stg_PTREC_WITHOUTK_info, CCS_SYSTEM);
            entry->tvar = tvar;
            entry->read_value = val;
            entry->next = trec->read_set;
            trec->read_set = entry;
            trec->capture_freq--;
        }
    }else{//filter the read set
        //TRACE("Read from tvar %p, filtering read set");
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

void p_stmWriteTVar(Capability *cap,
                    StgPTRecHeader *trec,
                    StgTVar *tvar,
                    StgClosure *new_value) {
    TRACE("Writing to tvar %p");
    StgWriteSet * newEntry = (StgWriteSet *) allocate(cap, sizeofW(StgWriteSet));
    SET_HDR(newEntry , &stg_WRITE_SET_info, CCS_SYSTEM);
    newEntry->tvar = tvar;
    newEntry->val = new_value;
    newEntry->next = trec->write_set;
    trec->write_set = newEntry;
}

void stmPushWriteSet(Capability * cap, StgWriteSet * write_set){
    while(write_set != TO_WRITE_SET(NO_PTREC)){
        StgTVar * tvar = write_set->tvar;
        tvar->current_value = write_set->val;
        dirty_TVAR(cap,tvar);
        write_set = write_set->next;
    }
}

/*
 * This function validates the log.  If validation succeeds, then 
 * it returns "(StgClosure*)0".  If it fails, but was unable to 
 * find a safe checkpoint, then we return "(StgClosure*)1", 
 * otherwise we return a pointer to continuation to be "thrown to"
 * This is pretty ugly, but I'm not familiar enough with C-- to come
 * up with anything better...
 */
StgPTRecWithK * p_stmCommitTransaction(Capability *cap, StgPTRecHeader *trec) {
    unsigned long snapshot = trec->read_version;
    while (cas(&version_clock, snapshot, snapshot+1) != snapshot){ 
        StgPTRecWithK * checkpoint = validate(trec);
        if(checkpoint != (StgPTRecWithK *) PASTM_SUCCESS){
            //The validate function sets up the trec with the appropriate read/write sets
            return checkpoint;
        }
        snapshot = trec->read_version;
    }
    
    StgWriteSet * write_set = trec->write_set;
    stmPushWriteSet(cap, write_set);

    TRACE("Committing transaction, version clock is currently %lu, soon to be %lu (must currently be odd)\n", version_clock, snapshot+2);
    version_clock = snapshot + 2;//unlock clock
    return (StgPTRecWithK *)PASTM_SUCCESS;
}

/* -----------------------------------------------------------------------------
 * p_setAtomicallyFrameHelper
 * 
 * This function is called by the preadTVar# primitive.  It traverses the stack
 * leaving tso->sp referring to the enclosing atomically frame.  
 * When performing an eager abort, we should only need to pop one frame...
 * --------------------------------------------------------------------------- */
void p_setAtomicallyFrameHelper(Capability *cap, StgTSO *tso){
  StgPtr           p, next;
  StgRetInfoTable *info;

  p = tso->stackobj->sp;
  while (1) {
    info = get_ret_itbl((StgClosure *)p);
    next = p + stack_frame_sizeW((StgClosure *)p);
    switch (info->i.type) {

    case ATOMICALLY_FRAME:
        tso->stackobj->sp = p;
        return;

    case UNDERFLOW_FRAME: 
        tso->stackobj->sp = p;
        threadStackUnderflow(cap,tso);
        p = tso->stackobj->sp;
        continue;

    default:
      ASSERT(info->i.type != CATCH_FRAME);
      ASSERT(info->i.type != STOP_FRAME);
      p = next;
      continue;
    }
  }
}

/*
 * Retry the transaction, we check to see that we 
 * have something on our retry stack before entering
 * this function.  This will return the alternative
 * branch of the orElse and set the read set/write set
 * appropriately.  
 */
StgClosure * p_stmRetry(StgPTRecHeader * trec){
    StgClosure * alt = trec->retry_stack->alt;
    StgPTRecWithK * checkpoint = trec->retry_stack->read_set;
    trec->read_set = checkpoint->next;               //set read set
    trec->write_set = checkpoint->write_set;         //set write set
    trec->lastK = checkpoint->prev_k;                //set short path
    trec->retry_stack = trec->retry_stack->next;     //pop the retry stack

    StgInt numK = 0;
    StgPTRecWithK * ptr = checkpoint->prev_k;
    while(ptr != TO_WITHK(NO_PTREC)){
        numK++;
        ptr = ptr->prev_k;
    }

    trec->numK = numK;
    return alt;
}

//I think the header can be clean, since it isn't going to be pointing
//to anything in a younger generation
static StgTVar retryTV = {.header = {.info = &stg_TVAR_CLEAN_info /*, TODO: add profiling field*/}, 
                          .current_value = PASTM_SUCCESS, //dummy value
                          .first_watch_queue_entry = ((StgTVarWatchQueue *)(void *)&stg_END_STM_WATCH_QUEUE_closure),
                          .num_updates = 0};

void p_stmCatchRetry(Capability *cap, StgPTRecHeader * trec, 
                     StgClosure * alt, StgClosure * continuation){
    StgPTRecWithK * checkpoint = (StgPTRecWithK*)allocate(cap, sizeofW(StgPTRecWithK));
    SET_HDR(checkpoint, &stg_PTREC_WITHK_info, CCS_SYSTEM);
    checkpoint->tvar = &retryTV;
    checkpoint->read_value = PASTM_SUCCESS;
    checkpoint->write_set = trec->write_set;
    checkpoint->next = trec->read_set;
    checkpoint->continuation = continuation;
    checkpoint->prev_k = trec->lastK;
    checkpoint->is_retry = TRUE;
    trec->read_set = TO_WITHOUTK(checkpoint);
    trec->lastK = checkpoint;
    //NOTE: don't update the continuation count or the capture frequency
    
    StgPTRecOrElse * orelse = (StgPTRecOrElse*)allocate(cap, sizeofW(StgPTRecOrElse));
    SET_HDR(checkpoint, &stg_PTREC_OR_ELSE_info, CCS_SYSTEM);
    orelse->alt = alt;
    orelse->read_set = checkpoint;
    orelse->next = trec->retry_stack;
    trec->retry_stack = orelse;
}

