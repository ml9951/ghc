{-# LANGUAGE CPP #-} 

--To build with full abort: ghc -threaded -DFULL_ABORT ll.hs
#ifdef FULL_ABORT
import Control.Concurrent.STM hiding (check)   --full abort STM
#else
import Control.Concurrent.PASTM hiding (check) --partial abort STM
#endif

import Prelude hiding (lookup)
import GHC.Conc(numCapabilities, forkIO)
import Control.Concurrent.MVar
import STMStats
import Data.Array
import System.Random
import Control.Monad.Random
import qualified Data.MultiSet as MS
import Data.Array.MArray

numTVars :: Int
numTVars = 100
numSamples = 10
numWrites = 10 
numIters = 200 --1000

type TVars = TArray Int Int

tvars :: STM TVars
tvars = newArray (0, numTVars) 0

sample :: Int -> Int -> RandT StdGen STM Int
sample 0 x = return x
sample i x = do
       tvars <- lift tvars
       rn <- getRandomR (0, numTVars)
       x <- lift $ readArray tvars rn
       sample (i-1) x

write :: Int -> RandT StdGen STM [Int]
write 0 = return []
write i = do
      tvars <- lift tvars
      rn <- getRandomR (0, numTVars)
      temp <- lift $ readArray tvars rn
      lift $ writeArray tvars rn (temp + 1)
      rands <- write (i-1)
      lift $ return $ rn : rands

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
     mvs <- mkThreads numCapabilities
     writes <- join mvs
     let freq = MS.toOccurList (MS.fromList writes)
     putStrLn ("Frequency = " ++ show freq)
     failed <- atomically $ check freq
     mapM_ (\(i,freq,actual) -> putStrLn(show i ++ ": Count should be " ++ show actual ++ ", but found " ++ show freq)) failed
     return()










