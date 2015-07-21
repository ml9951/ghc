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

numTVars = 100
numSamples = 10
numWrites = 10 
numIters = 200 --1000

newtype TArray i e = TArray (Array i (TVar e)) deriving (Eq, Typeable)

globalTV = newTVar 0

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

tvars :: STM TVars
tvars = newArray (0, numTVars) 0

sample :: Int -> Int -> RandT StdGen STM Int
sample 0 x = return x
sample i x = do
       rn <- getRandomR(0, numTVars)
       x <- lift (do  
            tvars <- tvars
            x <- readArray tvars rn
            return(x))
       sample (i-1) x

write :: Int -> RandT StdGen STM [Int]
write 0 = return []
write i = do
      rn <- getRandomR (0, numTVars)
      rands <- write (i-1)
      lift $ do
           tvars <- tvars
           temp <- readArray tvars rn
           let !new = temp + 1
           traceM ("Updating position " ++ show rn ++ " to " ++ show new)
           writeArray tvars rn new
           t <- globalTV
           x <- readTVar t
           writeTVar t (x+1)
           return $ rn : rands

threadLoop :: StdGen -> Int -> IO [Int]
threadLoop g 0 = return []
threadLoop g i = do
           (writes, g) <- atomically $ runRandT (sample numSamples 0 >>= \ _-> write numWrites) g
           rest <- threadLoop g (i-1)
           return(writes ++ rest)

mkThreads :: Int -> IO [MVar [Int]]
mkThreads 0 = return[]
mkThreads i = do
          gen <- newStdGen
          mv <- newEmptyMVar
          forkIO $ threadLoop gen numIters >>= \writes -> putMVar mv writes
          mvs <- mkThreads (i-1)
          return(mv : mvs)

join [] = return []
join (mv:mvs) = do
     writes <- takeMVar mv
     rest <- join mvs
     return(writes ++ rest)


check :: [(Int, Int)] -> STM [(Int, Int, Int)]
check [] = return []
check ((i, freq):rest) = do
      tvars <- tvars
      actual <- readArray tvars i
      if actual == freq
      then check rest
      else do
           failed <- check rest
           return((i, freq, actual):failed)

main = do
     putStrLn("Executing benchmark with " ++ whichSTM)
     mvs <- mkThreads numCapabilities
     writes <- join mvs
     let freq = MS.toOccurList (MS.fromList writes)
     failed <- atomically $ check freq
     mapM_ (\(i,freq,actual) -> putStrLn(show i ++ ": Count should be " ++ show freq ++ ", but found " ++ show actual)) failed
     x <- atomically $ do t <- globalTV; readTVar t
     putStrLn("x = " ++ show x)
     return()










