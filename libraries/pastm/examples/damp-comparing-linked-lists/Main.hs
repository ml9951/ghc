{-# LANGUAGE CPP, ForeignFunctionInterface #-} 

#ifdef STRAIGHT
import StraightForwardSTM
#elif defined(DISSECTED)
import DissectedSTM
#elif defined(LOCKCOUPLING)
import LockCoupling
#elif defined(CAS)
import CASLL
#else
#error Undefined Implementation
#endif


import System.Random(randomRs, mkStdGen)
import Control.Monad(foldM_)
import Control.Concurrent.MVar(takeMVar, putMVar, newEmptyMVar, MVar)
import Control.Concurrent(forkOn, getNumCapabilities)
import Text.Printf(printf)
import Dump

--            0-1          2           3-6
data Op = Lookup Int | Delete Int | Insert Int
numOps = 3000

threadLoop [] l = return()
threadLoop (r:rs) l = 
           if mod r 7 <= 1
           then find l r >>= \b -> threadLoop rs l
           else if mod r 7 == 2
                then delete l r >>= \b -> threadLoop rs l
                else addToTail l r >>= \b -> threadLoop rs l

mkThreads 0 l = return[]
mkThreads i l = do
          mv <- newEmptyMVar
          forkOn (i-1) $ do threadLoop (take numOps (randomRs (0, 100000) (mkStdGen i))) l; putMVar mv()
          mvs <- mkThreads (i-1) l
          return $ mv : mvs

join :: [MVar()] -> IO ()
join [] = return()
join (mv:mvs) = do
     takeMVar mv
     join mvs

main = do
#if defined(STRAIGHT) || defined(DISSECTED)
     putStrLn ("Executing benchmark with " ++ whichSTM)
#endif
     l <- newList
     let randoms = randomRs (0::Int, 100000) (mkStdGen 0)
     let (init, randoms') = splitAt 3000 randoms
     foldM_ (\b -> \a -> addToTail l a) () init
   --  numCapabilities <- getNumCapabilities
     start <- getTime
     mvs <- mkThreads 8 l
     join mvs
     end <- getTime
     printf "Time = %0.3f\n" (end - start :: Double)
#if defined(STRAIGHT) || defined(DISSECTED)
     printStats
#endif
     return()

foreign import ccall unsafe "hs_gettime" getTime :: IO Double