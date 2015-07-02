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
#define TRACE(_x...) debugTrace(DEBUG_stm, "STM: " _x)
#define KBOUND 20

static volatile unsigned long version_clock = 0;

static StgPTRecHeader * alloc_stg_ptrec_header(Capability * cap){
    StgPTRecHeader * ptrec;
    ptrec = (StgPTRecHeader *)allocate(cap, sizeofW(StgPTRecHeader));
    SET_HDR(ptrec , &stg_PTREC_HEADER_info, CCS_SYSTEM);
    ptrec->read_set = NULL;
    ptrec->lastK = NULL;
    ptrec->write_set = NULL;

    //get a read version
    ptrec->read_version = version_clock;
    while((ptrec->read_version & 1) != 0){
        ptrec->read_version = version_clock;
    }
    ptrec->capture_freq = ((unsigned long)1 << 32) + 1 ; 
    ptrec->numK = 0;
    return ptrec;
}

StgPTRecHeader * p_stmStartTransaction(Capability *cap) {
    StgPTRecHeader * t;
    TRACE("p_stmStartTransaction with %d tokens",
          cap -> transaction_tokens);
        
    t = alloc_stg_ptrec_header(cap);
    TRACE("stmStartTransaction()=%p", t);
    return t;
}

static StgBool should_abort = TRUE;

static void sanity_check(StgPTRecHeader * trec){
    //run forward

    int withKCount1, withKCount2;
    withKCount1 = 0;
    withKCount2 = 0;
    StgPTRecWithoutK * ptr = trec->read_set;
    while(ptr != NULL){
        if(ptr->header.info == WITHK_HEADER){
            withKCount1++;
            ptr = ptr->next;
        }else{
            ptr = ptr->next;
        }
    }

    //run backwards
    StgPTRecWithK * wptr = trec->lastK;
    while(wptr != NULL){
        if(wptr->header.info != WITHK_HEADER){
            printf("Error! found entry on short path without continuation\n");
            return;
        }
        withKCount2++;
        wptr = wptr->prev_k;
    }

    if(trec->numK != withKCount1 || trec->numK != withKCount2){
        printf("Sanity check failed: trec->numK = %ld, long path continuations = %d, short path continuations = %d\n", trec->numK, withKCount1, withKCount2);
    }

}

static StgPTRecWithK * validate(StgPTRecHeader * trec){
    sanity_check(trec);
    while(TRUE){
        unsigned long time = version_clock;
        if((time & 1) != 0){
            continue; //clock is locked
        }
        //validate read set
        StgPTRecWithoutK * ptr = trec->read_set;
        StgPTRecWithK *checkpoint = NULL;
        StgBool needCheckpoint = FALSE;
        int i = 0;
        while(ptr != NULL){
            if(ptr->header.info == WITHK_HEADER){ // this is a WithK entry
                StgPTRecWithK * withK = (StgPTRecWithK*)ptr;
                if(withK->read_value != withK->tvar->current_value){
                    checkpoint = withK;
                    needCheckpoint = FALSE;
                }else if(needCheckpoint){ //Valid and we need a checkpoint
                    checkpoint = withK;
                    needCheckpoint = FALSE;
                }
                if(should_abort && i > 20){
                    should_abort = FALSE;
                    return withK;
                }
            }else if(ptr->header.info == WITHOUTK_HEADER){
                if(ptr->read_value != ptr->tvar->current_value){
                    checkpoint = NULL;
                    needCheckpoint = TRUE;
                }
            }else{
                //something bad happened
            }
            ptr = ptr->next;
            i++;
        }
        if(needCheckpoint){ //no checkpoint found, but we need to abort
            return (StgPTRecWithK *) 1;
        }
        if(checkpoint != NULL){ //validation failed, but we found a checkpoint
            //try reading from this tvar
            StgTVar * tvar = checkpoint->tvar;
            StgClosure * val = tvar->current_value;
            if(time == version_clock){
                checkpoint->read_value = val; //apply the continuation to this in C--
                trec->read_set = (StgPTRecWithoutK*)checkpoint;
                trec->write_set = checkpoint->write_set;
                trec->lastK = checkpoint;
                StgInt64 capture_freq = trec->capture_freq & 0xFFFFFFFF00000000;
                trec->capture_freq = capture_freq | (capture_freq >> 32);
                return checkpoint;
            }else{
                continue; //revalidate the whole log
            }
        }

        if(time == version_clock){
            trec->read_version = time;
            return (StgPTRecWithK *) 0;
        }
    }
}

StgClosure * p_stmReadTVar(Capability * cap, StgPTRecHeader * trec, 
                           StgTVar * tvar, StgClosure * k){
    StgWriteSet * ws = trec->write_set;

    while(ws != NULL){
        if(ws->tvar == tvar){
            return ws->val;
        }
        ws = ws->next;
    }

    //Not found in write set
    StgClosure * val = tvar->current_value;
    while(trec->read_version != version_clock){
        StgPTRecWithK * checkpoint = validate(trec);
        if(checkpoint != (StgPTRecWithK *) 0){
            //Abort!
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
            trec->read_set = (StgPTRecWithoutK*)entry;
            trec->lastK = entry;
            trec->capture_freq |= (trec->capture_freq >> 32);
            trec->numK++;
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
        while(ptr != NULL){
            if(ptr->prev_k != NULL){
                StgPTRecWithK * dropped = ptr->prev_k;
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
    StgWriteSet * newEntry = (StgWriteSet *) allocate(cap, sizeofW(StgWriteSet));
    SET_HDR(newEntry , &stg_WRITE_SET_info, CCS_SYSTEM);
    newEntry->tvar = tvar;
    newEntry->val = new_value;
    newEntry->next = trec->write_set;
    trec->write_set = newEntry;
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
    while (should_abort || cas(&version_clock, snapshot, snapshot+1) != snapshot){ 
        StgPTRecWithK * checkpoint = validate(trec);
        if(checkpoint != (StgPTRecWithK *) 0){
            //The validate function sets up the trec with the appropriate read/write sets
            return checkpoint;
        }
        snapshot = trec->read_version;
    }
    
    StgWriteSet * write_set = trec->write_set;
    while(write_set != NULL){
        StgTVar * tvar = write_set->tvar;
        tvar->current_value = write_set->val;
        dirty_TVAR(cap,tvar);
        write_set = write_set->next;
    }
    return (StgPTRecWithK *)0;
}



