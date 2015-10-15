{-# LANGUAGE CPP, MagicHash, UnboxedTuples, ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Control.TL2.STM
(
    readTVar,
    writeTVar,
    atomically,
    STM(..),
--    printStats,
    newTVar,
    module Control.Common.STM
)
where

--add TVar TVar# newTVar# newTVarIO readTVarIO
import GHC.Base(State#, RealWorld, IO(..), ap)
import GHC.Prim(Any, unsafeCoerce#, newTL2TVar#, tl2_writeTVar#, tl2_readTVar#, tl2_atomically# )
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
newTVar x = STM $ \s -> case unsafeCoerce# newTL2TVar# x s of
                              (# s', tv #) -> (# s', TVar tv #)

readTVar :: TVar a -> STM a
readTVar (TVar tv) = STM $  \s-> unsafeCoerce# tl2_readTVar# tv s 

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tv) a = STM $ \s -> 
          case unsafeCoerce# tl2_writeTVar# tv a s of
               (# s', tv #) -> (# s', () #)

atomically :: STM a -> IO a
atomically (STM c) = IO (\s -> unsafeCoerce# tl2_atomically# c s)
{-
foreign import prim safe "stg_tl2_atomicallyzh" atomically# 
        :: State# s -> (# State# s, Any() #)
--        :: Any() -> (# State# s, Any() #)
--          :: (State# RealWorld -> (# State# RealWorld, a #) )
--             -> State# RealWorld -> (# State# RealWorld, a #)

foreign import prim safe "stg_tl2_readTVarzh" readTVar#
        :: Any() -> State# s -> (# State# s, a #)

foreign import prim safe "stg_tl2_writeTVarzh" writeTVar#
        :: Any() -> Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import ccall "c_tl2_printSTMStats" printStats :: IO ()
-}
