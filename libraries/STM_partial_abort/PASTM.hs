{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types#-}

module PASTM
(
    readTVar,
    writeTVar,
    atomically,
    STM(..),
    getStats,
    mergeStats,
    --The following are just re-exporting from the original STM
    newTVarIO,   
    readTVarIO, 
    TVar(..),    
    newTVar         
)
where

import GHC.Conc.Sync(TVar(..), readTVarIO, newTVarIO)
import GHC.Base
import Data.Map(fromList, unionWith, Map)

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

getStats :: IO (Map String [Int])
getStats = 
         IO $ \s# -> case getStats# s# of
                         (# s'#, s1#, s2#, s3#, s4#, s5# #) ->
                            (# s'#, fromList [("Eager Partial Aborts", [I# s1#]), 
                                              ("Eager Full Aborts", [I# s2#]),
                                              ("Commit Time Partial Aborts", [I# s3#]),
                                              ("Commit Time Full Aborts", [I# s4#]),
                                              ("Successfull Commits", [I# s5#])
                                             ]
                            #)

mergeStats :: Map String [Int] -> Map String [Int] -> Map String [Int]
mergeStats s1 s2 = unionWith (++) s1 s2