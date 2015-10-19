{-# LANGUAGE CPP, MagicHash, UnboxedTuples, ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Control.ChunkedTL2.STM
(
    readTVar,
    writeTVar,
    atomically,
    STM(..),
    printStats,
    newTVar,
    module Control.Common.STM
)
where

import GHC.Base(State#, RealWorld, IO(..), ap)
import GHC.Prim(Any, unsafeCoerce# )
import GHC.Conc.Sync(TVar(..))
import Control.Common.STM

newtype STM a = STM {unSTM :: State# RealWorld -> (# State# RealWorld, a #)}

instance Monad STM where
    return a = STM $ \s -> (# s, a #)
    m >>= k = STM $ \s -> let (# s', t #) = unSTM m s in unSTM (k t) s'

instance Applicative STM where
    (<*>) = ap
    pure  = return

instance  Functor STM where
    fmap f m = m >>= (return . f)

newTVar :: a -> STM (TVar a)
newTVar x = STM $ \s -> case unsafeCoerce# newTVar# x s of
                              (# s', tv #) -> (# s', TVar tv #)

readTVar :: TVar a -> STM a
readTVar (TVar tv) = STM $  \s-> unsafeCoerce# readTVar# tv s 

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tv) a = STM $ \s -> 
          case unsafeCoerce# writeTVar# tv a s of
               (# s', tv #) -> (# s', () #)

atomically :: STM a -> IO a
atomically (STM c) = IO (\s -> unsafeCoerce# atomically# c s)

foreign import prim safe "stg_tl2_atomicallyzh" atomically# 
        :: Any() -> State# s -> (# State# s, Any() #)

foreign import prim safe "stg_tl2_readTVarzh" readTVar#
        :: Any() -> State# s -> (# State# s, a #)

foreign import prim safe "stg_tl2_writeTVarzh" writeTVar#
        :: Any() -> Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import ccall "c_tl2_printSTMStats" printStats :: IO ()
