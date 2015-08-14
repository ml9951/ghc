{-# LANGUAGE CPP, MagicHash, UnboxedTuples, Rank2Types, ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes#-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Full.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PASTM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.  
--
--  * This contains the core STM operations
--
-----------------------------------------------------------------------------

module Control.Partial.STM
(
    readTVar,
    writeTVar,
    atomically,
    STM(..),
    printStats,
    retry,
    orElse,
    --The following are just re-exporting from the original STM
    TVar(..),    
    newTVar       
)
where


import GHC.Conc.Sync(TVar(..))
import GHC.Base(State#, RealWorld, IO(..), ap, newTVar#, TVar#)
import GHC.Prim(Any, unsafeCoerce# )

newtype STM a = STM {unSTM :: forall r . --r is the type of the final result
                                 (a -> State# RealWorld -> (# State# RealWorld, r #)) -> --Continuation
                                 State# RealWorld ->                                     --Incoming State
                                 (# State# RealWorld, r #)}                              --New state and result

instance Monad STM where
    return a = STM $ \c -> c a
    m >>= k = STM $ \c -> unSTM m (\a -> \s' -> unSTM (k a) c s')

instance Applicative STM where
    (<*>) = ap
    pure  = return

instance  Functor STM where
    fmap f m = m >>= (return . f)

readTVar :: TVar a -> STM a
readTVar (TVar tv) = STM $ \c -> \s-> case unsafeCoerce# readTVar# tv c s of
                                        (# s', t #) -> c t s'

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tv) a = STM $ \c -> \s -> 
          case unsafeCoerce# writeTVar# tv a s of
               s' -> c () s'

newTVar :: a -> STM (TVar a)
newTVar x = STM $ \c -> \s -> case newTVar# x s of
                                (# s', tv #) -> c (TVar tv) s'

initK :: a -> State# RealWorld -> (# State# RealWorld, a #)
initK a s = (# s, a #)

atomically :: STM a -> IO a
atomically (STM c) = IO (\s -> unsafeCoerce# atomically# (c initK) s)


-- |Retry execution of the current memory transaction because it has seen
-- values in TVars which mean that it should not continue (e.g. the TVars
-- represent a shared buffer that is now empty).  The implementation may
-- block the thread until one of the TVars that it has read from has been
-- udpated. (GHC only)
retry :: STM a
retry = STM $ \c -> \s# -> pretry# s#

-- |Compose two alternative STM actions (GHC only).  If the first action
-- completes without retrying then it forms the result of the orElse.
-- Otherwise, if the first action retries, then the second action is
-- tried in its place.  If both actions retry then the orElse as a
-- whole retries.
orElse :: STM a -> STM a -> STM a
orElse (STM m) e = STM $ \c -> \s ->    
        case unsafeCoerce# pcatchRetry# (m (unsafeCoerce# popRetry#)) (unSTM e initK) s of
               (# s', t #) -> c t s'

foreign import prim safe "stg_partial_atomicallyzh" atomically# 
        :: Any() -> State# s -> (# State# s, Any() #)

foreign import prim safe "stg_partial_readTVarzh" readTVar#
        :: TVar# s a -> Any()
            -> State# s -> (# State# s, a #)

foreign import prim safe "stg_partial_writeTVarzh" writeTVar#
        :: TVar# RealWorld a -> Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #)

foreign import ccall "pa_printSTMStats" printStats :: IO ()

foreign import prim safe "stg_partial_retryzh" pretry#
        :: State# RealWorld -> (# State# RealWorld, a #)

foreign import prim safe "stg_partial_catchRetryzh" pcatchRetry#
        :: Any() -> Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import prim safe "stg_partial_popRetry" popRetry#
        :: Any() -> State# RealWorld -> (# State# RealWorld, a #)
