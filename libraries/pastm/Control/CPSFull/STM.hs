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
-- abstraction.  See
--
--  * This contains the core STM operations
--
-----------------------------------------------------------------------------

module Control.CPSFull.STM
(
    readTVar,
    writeTVar,
    atomically,
    atomically',
    STM(..),
    printStats,
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

atomically' :: STM a -> Word -> IO a
atomically' (STM c) event = IO $ \s -> unsafeCoerce# atomically'# (c initK) event s

foreign import prim safe "stg_full_atomicallyzhWithEvent" atomically'#
        :: Any() -> Any() -> State# RealWorld -> (# State# s, Any() #)
        
foreign import prim "stg_full_atomicallyzh" atomically# 
        :: Any() -> State# s -> (# State# s, Any() #)

foreign import prim "stg_full_readTVarzh" readTVar#
        :: TVar# s a -> Any() -> State# s -> (# State# s, a #)

foreign import prim "stg_full_writeTVarzh" writeTVar#
        :: TVar# RealWorld a -> Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #)


foreign import ccall "fa_printSTMStats" printStats :: IO()
