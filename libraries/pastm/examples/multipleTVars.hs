{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-} 

#ifdef STMHASKELL
import Control.STMHaskell.STM     --full abort STM (STM Haskell)
#elif defined(FABORT)
import Control.Full.STM           --full abort STM (NoRec)
#else
import Control.Partial.STM
#endif

import Prelude hiding (lookup)
import GHC.Conc(numCapabilities, forkIO)
import Control.Concurrent.MVar
import System.Random
import Control.Monad.Random
import qualified Data.MultiSet as MS
import Data.Array.MArray
import Dump

import Data.Array (Array, bounds)
import Data.Array.Base (listArray, arrEleBottom, unsafeAt, MArray(..),
                        IArray(numElements))
import Data.Ix (rangeSize)
import Data.Typeable (Typeable)

#ifdef STMHASKELL
whichSTM = "STM Haskell"
#elif defined(FABORT)
whichSTM = "Full Abort NoRec"
#else
whichSTM = "Partial Abort NoRec"
#endif

numTVars = (100 :: Int)
numSamples = 10
numWrites = 10 
numIters = (200 :: Int) --1000

newtype TArray i e = TArray (Array i (TVar e)) deriving (Eq, Typeable)

instance MArray TArray e STM where
    getBounds (TArray a) = return (bounds a)
    newArray b e = do
        a <- rep (rangeSize b) (newTVar e)
        return $ TArray (listArray b a)
    newArray_ b = do
        a <- rep (rangeSize b) (newTVar arrEleBottom)
        return $ TArray (listArray b a)
    unsafeRead (TArray a) i = readTVar $ unsafeAt a i
    unsafeWrite (TArray a) i e = writeTVar (unsafeAt a i) e
    getNumElements (TArray a) = return (numElements a)

-- | Like 'replicateM' but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM' the returned list is in reversed order.
-- This doesn't matter though since this function is only used to create
-- arrays with identical elements.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          !x <- m
          go (i-1) (x:xs)

type TVars = TArray Int Int

sample tvars [] x = return x
sample tvars (r:rs) x = do
       x <- readArray tvars r
       sample tvars rs x

write tvars [] = return ()
write tvars (r:rs) = do
      temp <- readArray tvars r
      traceM ("Incrementing tvar " ++ show r ++ " from " ++ show temp ++ " to " ++ show (temp+1))
      writeArray tvars r (temp+1)
      write tvars rs

threadLoop :: TVars -> [Int] -> Int -> IO [Int]
threadLoop tvars g 0 = return []
threadLoop tvars g i = do
           (samples, g1) <- return $ splitAt numSamples g
           (writes, g2) <- return $ splitAt numWrites g1
           atomically $ sample tvars samples 0 >>= \_ -> write tvars writes
           rest <- threadLoop tvars g2 (i-1)
           return(writes ++ rest)

mkThreads tvars 0 = return[]
mkThreads tvars i = do
          mv <- newEmptyMVar
          forkIO $ threadLoop tvars (randomRs (0, numTVars) (mkStdGen i)) numIters >>= \writes -> putMVar mv writes
          mvs <- mkThreads tvars (i-1)
          return(mv : mvs)

join [] = return []
join (mv:mvs) = do
     writes <- takeMVar mv
     rest <- join mvs
     return(writes ++ rest)


check :: TVars -> [(Int, Int)] -> STM [(Int, Int, Int)]
check tvars [] = return []
check tvars ((i, freq):rest) = do
      actual <- readArray tvars i
      if actual == freq
      then check tvars rest
      else do
           failed <- check tvars rest
           return((i, freq, actual):failed)

main = do
     putStrLn("Executing benchmark with " ++ whichSTM)  
     tvars <- atomically $ newArray (0, numTVars) 0
     mvs <- mkThreads tvars numCapabilities
     writes <- join mvs
     let freq = MS.toOccurList (MS.fromList writes)
     failed <- atomically $ check tvars freq
     mapM_ (\(i,freq,actual) -> putStrLn(show i ++ ": Count should be " ++ show freq ++ ", but found " ++ show actual)) failed
     putStrLn(show writes)
     return()








