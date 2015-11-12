{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

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
    retry,
    orElse,
    modifyTVar,
    modifyTVar',
    module Control.Common.STM
)
where

import GHC.Base(State#, RealWorld, IO(..), ap)
import GHC.Prim(Any, unsafeCoerce#, newTL2TVar#, tl2_writeTVar#, tl2_readTVar#, tl2_atomically#, tl2_retry#, tl2_popRetry#, tl2_catchRetry# )
import GHC.Conc.Sync(TVar(..))
import Control.Common.STM

newtype STM a = STM {unSTM :: State# RealWorld -> (# State# RealWorld, a #)}

instance Monad STM where
    return a = STM $ \s# -> (# s#, a #)
    (STM m) >>= k = STM $ \s# -> 
        case m s# of
            (# s'#, a #) -> unSTM (k a) s'#

instance Applicative STM where
    (<*>) = ap
    pure  = return

instance  Functor STM where
    fmap f m = m >>= (return . f)

newTVar :: a -> STM (TVar a)
newTVar x = STM $ \s# ->
    case newTL2TVar# x s# of
        (# s'#, tv# #) -> (# s'#, TVar tv# #)

readTVar :: TVar a -> STM a
readTVar (TVar tv#) = STM $ \s# -> tl2_readTVar# tv# s#

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tv#) a = STM $ \s1# ->
    case tl2_writeTVar# tv# a s1# of
        s2# -> (# s2#, () #)

atomically :: STM a -> IO a
atomically (STM c) = IO (\s# -> tl2_atomically# c s#)

retry :: STM a
retry = STM $ \s -> tl2_retry# s

popRetry :: a -> STM a
popRetry x = STM $ \s -> tl2_popRetry# x s

orElse :: STM a -> STM a -> STM a
orElse (STM a) (STM b) = STM $ \s -> tl2_catchRetry# a b s 

-- Like 'modifyIORef' but for 'TVar'.
-- | Mutate the contents of a 'TVar'. /N.B./, this version is
-- non-strict.
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
    x <- readTVar var
    writeTVar var (f x)
{-# INLINE modifyTVar #-}


-- | Strict version of 'modifyTVar'.
modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x
{-# INLINE modifyTVar' #-}

{-
foreign import prim safe "stg_tl2_atomicallyzh" atomically# 
        :: State# s -> (# State# s, Any() #)
--        :: Any() -> (# State# s, Any() #)
--          :: (State# RealWorld -> (# State# RealWorld, a #) )
--             -> State# RealWorld -> (# State# RealWorld, a #)
        :: Any() -> State# s -> (# State# s, Any() #)

foreign import prim safe "stg_tl2_readTVarzh" readTVar#
        :: Any() -> State# s -> (# State# s, a #)

foreign import prim safe "stg_tl2_writeTVarzh" writeTVar#
        :: Any() -> Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import ccall "c_tl2_printSTMStats" printStats :: IO ()
-}
