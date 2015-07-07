{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, BangPatterns#-}

module PASTM
(
    readTVar,
    writeTVar,
    atomically,
    STM(..),
    --The following are just re-exporting from the original STM
    newTVarIO,   
    readTVarIO, 
    TVar(..),    
    newTVar         
)
where

import GHC.Conc.Sync(TVar(..), readTVarIO, newTVarIO)
import GHC.Base
import GHC.Exts
import Dump
import GHC.IO

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

-- +DEBUG
-- A datatype that has the same layout as Word and so can be casted to it.
data Ptr' a = Ptr' a

-- Any is a type to which any type can be safely unsafeCoerced to.
aToWord# :: Any -> Word#
aToWord# a = let !mb = Ptr' a in case unsafeCoerce# mb :: Word of W# addr -> addr

unsafeAddr :: a -> Int
unsafeAddr a = I# (word2Int# (aToWord# (unsafeCoerce# a)))
-- -DEBUG


readTVar :: TVar a -> STM a
readTVar (TVar tv) = STM $ \c -> \s-> case (preadTVar# tv c s) of
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
