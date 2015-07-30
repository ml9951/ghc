{-# LANGUAGE CPP, MagicHash, UnboxedTuples, Rank2Types, ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes #-}

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

module Control.FullTL2.STM
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

--add TVar TVar# newTVar# newTVarIO readTVarIO
import GHC.Base(State#, RealWorld, IO(..), ap)
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

newtype TVar a = TVar (Any())

readTVar :: TVar a -> STM a
readTVar (TVar tv) = STM $ \c -> \s-> case unsafeCoerce# readTVar# tv c s of
                                        (# s', t #) -> c t s'

readTVarIO :: TVar a -> IO a
readTVarIO (TVar tv) = IO $ \s-> unsafeCoerce# readTVarIO# tv s 

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tv) a = STM $ \c -> \s -> 
          case unsafeCoerce# writeTVar# tv a s of
               (# s', tv #) -> c () s'

newTVarIO :: a -> IO (TVar a)
newTVarIO x = IO $ \s -> unsafeCoerce# newTVar# x s 

newTVar :: a -> STM (TVar a)
newTVar x = STM $ \c -> \s -> case unsafeCoerce# newTVar# x s of
                                (# s', tv #) -> c (TVar tv) s'

initK :: a -> State# RealWorld -> (# State# RealWorld, a #)
initK a s = (# s, a #)

atomically :: STM a -> IO a
atomically (STM c) = IO (\s -> unsafeCoerce# atomically# (c initK) s)

foreign import prim safe "stg_partial_atomicallyzh" atomically# 
        :: Any() -> State# s -> (# State# s, Any() #)

foreign import prim safe "stg_partial_readTVarzh" readTVar#
        :: Any() -> Any() -> State# s -> (# State# s, a #)

foreign import prim safe "stg_partial_writeTVarzh" writeTVar#
        :: Any() -> Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import ccall "pa_printSTMStats" printStats :: IO ()

foreign import prim safe "stg_newTL2TVarzh" newTVar#
        :: Any() -> State# RealWorld -> (# state# RealWorld, a #) 

foreign import prim safe "stg_readTL2TVarIOzh" readTVarIO#
        :: Any() -> State# s -> (# State# s, a #)

