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
import Control.Exception
import Dump

iters = 500

threadLoop :: Int -> TVar Int -> IO()
threadLoop 0 c = return()
threadLoop i c = do
           atomically $ do t <- readTVar c; writeTVar c (t+1)
           threadLoop (i-1) c

mkThreads :: TVar Int -> Int -> IO [MVar ()]
mkThreads c 0 = return []
mkThreads c i = do
          mv <- newEmptyMVar
          forkIO (do threadLoop iters c; putMVar mv())
          mvs <- mkThreads c (i-1)
          return(mv:mvs)

join :: [MVar()] -> IO ()
join [] = return()
join (mv:mvs) = do
     takeMVar mv
     join mvs
     return()

main = do
     counter <- newTVarIO 0
     mvars <- mkThreads counter numCapabilities
     join mvars --wait for everyone to finish adding to list
     count <- readTVarIO counter
     putStrLn("Count is " ++ show count)
     return()
