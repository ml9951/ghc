/*----------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * STM interface definition
 *
 *----------------------------------------------------------------------*/

#ifndef STM_H
#define STM_H

#ifdef THREADED_RTS
#define STM_FG_LOCKS
#else
#define STM_UNIPROC
#endif

#include "BeginPrivate.h"

StgPTRecHeader *p_stmStartTransaction(Capability *);

StgClosure * p_stmReadTVar(Capability *, StgPTRecHeader *, 
                           StgTVar *, StgClosure *);

void p_stmWriteTVar(Capability *, StgPTRecHeader *,
                    StgTVar *, StgClosure *);

StgPTRecWithK * p_stmCommitTransaction(Capability *, StgPTRecHeader *);

void p_setAtomicallyFrameHelper (Capability *, StgTSO *);

#define NO_PTREC ((StgPTRecHeader *)(void *)&stg_NO_PTREC_closure)
#define WITHK_HEADER &stg_PTREC_WITHK_info
#define WITHOUTK_HEADER &stg_PTREC_WITHOUTK_info

#include "EndPrivate.h"

/*----------------------------------------------------------------------*/
#endif /* PartialAbortSTM_H */

