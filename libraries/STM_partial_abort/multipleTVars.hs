{-# LANGUAGE CPP #-} 

--To build with full abort: ghc -threaded -DFULL_ABORT ll.hs
#ifdef FULL_ABORT
import GHC.Conc.Sync   --full abort STM
#else
import PASTM           --partial abort STM
#endif

import Prelude hiding (lookup)
import GHC.Conc(numCapabilities, forkIO)
import Control.Concurrent.MVar
import STMStats
import Data.Array
import System.Random
import Control.Monad.Random
import qualified Data.MultiSet as MS

numTVars :: Int
numTVars = 100
numSamples = 10
numWrites = 10 
numIters = 1000

test :: RandT StdGen STM Int
test = do
     x <- lift $ newTVar 0
     r <- getRandomR (0, 12)
     return r



test2 :: STM Int
test2 = evalRandT test (mkStdGen 0)

type TVars = Array Int (TVar Int)

--tvars :: Array Int (STM (TVar Int))
--tvars = listArray (0, numTVars) (repeat (newTVar 0))

sample :: TVars -> Int -> Int -> RandT StdGen STM Int
sample tvars 0 x = return x
sample tvars i x = do
       rn <- getRandomR (0, numTVars)
       tv <- lift $ tvars ! rn
       x <- lift $ readTVar tv
       sample tvars (i-1) x

write :: TVars -> Int -> RandT StdGen STM [Int]
write tvars 0 = return []
write tvars i = do
      rn <- getRandomR (0, numTVars)
      tv <- lift $  tvars ! rn
      temp <- lift $ readTVar tv
      lift $ writeTVar tv (temp+1)
      rands <- write tvars (i-1)
      lift $ return $ rn : rands

threadLoop :: TVars -> StdGen -> Int -> IO [Int]
threadLoop tvars g 0 = return []
threadLoop tvars g i = do
           (writes, g) <- atomically $ runRandT (sample tvars numSamples 0 >>= \ _-> write tvars numWrites) g
           rest <- threadLoop tvars g (i-1)
           return(writes ++ rest)

mkThreads :: TVars -> Int -> IO [MVar [Int]]
mkThreads tvars 0 = return[]
mkThreads tvars i = do
          gen <- newStdGen
          mv <- newEmptyMVar
          forkIO $ threadLoop tvars gen numIters >>= \writes -> putMVar mv writes
          mvs <- mkThreads tvars (i-1)
          return(mv : mvs)

join [] = return []
join (mv:mvs) = do
     writes <- takeMVar mv
     rest <- join mvs
     return(writes ++ rest)

check :: [(Int, Int)] -> IO()
check [] = return()
check ((i, freq):rest) = do
      


main = do
     tvars <- 
     mvs <- mkThreads numCapabilities
     writes <- join mvs
     let freq = MS.toOccurList (MS.fromList writes)
     putStrLn("Frequency = " ++ show freq)
     return()










