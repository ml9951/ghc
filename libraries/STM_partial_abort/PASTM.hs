{-# LANGUAGE MagicHash, UnboxedTuples#-}

module PASTM
(
    readTVar,
    writeTVar,
    newTVar,
    atomically,
    TVar(..),
    STM,
    STM'(..),
    newTVarIO,  --No need to change from original STM
    readTVarIO  --Same here
)
where

import GHC.Conc.Sync(TVar(..), readTVarIO, newTVarIO)
import GHC.Base

newtype STM' r a = STM' {unSTM :: (State# RealWorld -> a -> (# State# RealWorld, r #)) -> --Continuation
                                  State# RealWorld ->                                     --Incoming State
                                  (# State# RealWorld, r #)}                              --New state and result

instance Monad (STM' r) where
    return a = STM' $ \c -> \s -> c s a
    m >>= k = STM' $ \c -> \s -> unSTM m (\s' -> \a -> unSTM (k a) c s') s

instance Applicative (STM' r) where
    (<*>) = ap
    pure  = return

instance  Functor (STM'  r) where
    fmap f m = m >>= (return . f)

readTVar :: TVar a -> STM' r a
readTVar (TVar tv) = STM' $ \c -> \s-> case preadTVar# tv c s of
                                        (# s', t #) -> c s' t

writeTVar :: TVar a -> a -> STM' r ()
writeTVar (TVar tv) a = STM' $ \c -> \s -> c (pwriteTVar# tv a s) ()

newTVar :: a -> STM' r (TVar a)
newTVar x = STM' $ \c -> \s -> case newTVar# x s of
                                (# s', tv #) -> c s' (TVar tv)

initK :: State# RealWorld -> a -> (# State# RealWorld, a #)
initK s a = (# s, a #)

type STM a = STM' a a

atomically :: STM a -> IO a
atomically (STM' c) = IO (\s -> patomically# (c initK) s)

test = do
     x <- newTVar 0
     writeTVar x 123
     y <- readTVar x
     return(y)

main = do
     x <- atomically test
     print("Result = " ++ show x)
     return()





