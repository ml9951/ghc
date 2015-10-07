{-# LANGUAGE CPP, MagicHash, UnboxedTuples, ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes#-}

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

module Control.NoRec.STM
(
    readTVar,
    writeTVar,
    atomically,
    STM(..),
    printStats,
    TVar(..),    
    newTVar       
)
where

import GHC.Conc.Sync(TVar(..))
import GHC.Base(State#, RealWorld, IO(..), ap, newTVar#, TVar#)
import GHC.Prim(Any, unsafeCoerce# )

newtype STM a = STM {unSTM :: State# RealWorld ->                                     --Incoming State
                              (# State# RealWorld, a #)}                              --New state and result

instance Monad STM where
    return a = STM $ \s -> (# s, a #)
    m >>= k =  STM $ \s -> case unSTM m s of
                             (# s', t #) -> unSTM (k t) s'
instance Applicative STM where
    (<*>) = ap
    pure  = return

instance  Functor STM where
    fmap f m = m >>= (return . f)

readTVar :: TVar a -> STM a
readTVar (TVar tv) =  STM $ \s-> unsafeCoerce# readTVar# tv s

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tv) a = STM $ \s -> 
          case unsafeCoerce# writeTVar# tv a s of
               (# s', _ #) -> (# s', () #)

newTVar :: a -> STM (TVar a)
newTVar x = STM $ \s -> case newTVar# x s of
                            (# s', tv #) -> (# s', TVar tv #)

atomically :: STM a -> IO a
atomically (STM c) = IO (\s -> unsafeCoerce# atomically# c s)

foreign import prim "stg_norec_atomicallyzh" atomically# 
        :: Any() -> State# s -> (# State# s, Any() #)
{-         FFI won't accept this type...
        :: (State# RealWorld -> (# State# RealWorld , b #) )
            -> State# RealWorld -> (# State# RealWorld, b #)
-} 

foreign import prim "stg_norec_readTVarzh" readTVar#
        :: TVar# s a -> State# s -> (# State# s, a #)

foreign import prim "stg_norec_writeTVarzh" writeTVar#
        :: TVar# RealWorld a -> Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #)


foreign import ccall "c_norec_printSTMStats" printStats :: IO()