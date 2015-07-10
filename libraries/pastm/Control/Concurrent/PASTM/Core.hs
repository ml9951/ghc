{-# LANGUAGE CPP, MagicHash, UnboxedTuples, Rank2Types#-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.PASTM.Core
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

module Control.Concurrent.PASTM.Core
(
    readTVar,
    writeTVar,
    atomically,
    STM(..),
    retry,
    orElse,
    --The following are just re-exporting from the original STM
    newTVarIO,   
    readTVarIO, 
    TVar(..),    
    newTVar         
)
where

import GHC.Conc.Sync(TVar(..), readTVarIO, newTVarIO)
import GHC.Base

newtype STM a = STM {unSTM :: forall r . --r is the type of the final result
                                 (State# RealWorld -> a -> (# State# RealWorld, r #)) -> --Continuation
                                 State# RealWorld ->                                     --Incoming State
                                 (# State# RealWorld, r #)}                              --New state and result

instance Monad STM where
    return a = STM $ \c -> \s -> c s a
    m >>= k = STM $ \c -> \s -> unSTM m (\s' -> \a -> unSTM (k a) c s') s

instance Applicative STM where
    (<*>) = ap
    pure  = return

instance  Functor STM where
    fmap f m = m >>= (return . f)

readTVar :: TVar a -> STM a
readTVar (TVar tv) = STM $ \c -> \s-> case preadTVar# tv c s of
                                        (# s', t #) -> c s' t

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tv) a = STM $ \c -> \s -> c (pwriteTVar# tv a s) ()

newTVar :: a -> STM (TVar a)
newTVar x = STM $ \c -> \s -> case newTVar# x s of
                                (# s', tv #) -> c s' (TVar tv)

initK :: State# RealWorld -> a -> (# State# RealWorld, a #)
initK s a = (# s, a #)

atomically :: STM a -> IO a
atomically (STM c) = IO (\s -> patomically# (c initK) s)

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
orElse (STM m) e = STM $ \c -> \s -> pcatchRetry# (m c) (unSTM e c) s




