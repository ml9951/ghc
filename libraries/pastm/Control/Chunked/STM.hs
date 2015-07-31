{-# LANGUAGE CPP, MagicHash, UnboxedTuples, Rank2Types, ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes#-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Ordered.STM
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

module Control.Chunked.STM
(
    readTVar,
    writeTVar,
    atomically,
    STM(..),
    printStats,
  --  retry,
  --  orElse,
    --The following are just re-exporting from the original STM
    newTVarIO,   
    readTVarIO, 
    TVar(..),    
    newTVar       
)
where


import GHC.Conc.Sync(TVar(..), readTVarIO, newTVarIO)
import GHC.Base(State#, RealWorld, IO(..), ap, newTVar#, TVar#)
import GHC.Prim(Any, unsafeCoerce# )

newtype STM a = STM {unSTM :: forall r . --r is the type of the final result
                                 (a -> State# RealWorld -> (# State# RealWorld, r #)) -> --Continuation
                                 State# RealWorld ->                                     --Incoming State
                                 (# State# RealWorld, r #)}                              --New state and result

instance Monad STM where
    return a = STM $ \c -> \s -> c a s
    m >>= k = STM $ \c -> \s -> unSTM m (\a -> \s' -> unSTM (k a) c s') s

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

foreign import prim safe "stg_ordered_atomicallyzh" atomically# 
        :: Any() -> State# s -> (# State# s, Any() #)

foreign import prim safe "stg_ordered_readTVarzh" readTVar#
        :: TVar# s a -> Any()
            -> State# s -> (# State# s, a #)

foreign import prim safe "stg_ordered_writeTVarzh" writeTVar#
        :: TVar# RealWorld a -> Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #)

foreign import ccall "ord_printSTMStats" printStats :: IO ()

