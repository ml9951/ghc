{-# LANGUAGE CPP #-} 

#ifdef STMHASKELL
import Control.Concurrent.STM hiding(check)   --full abort STM
#elif defined(FABORT)
import Control.Full.STM           --full abort STM (NoRec)
#else
--import Control.Concurrent.PASTM.Core
import Control.Partial.STM
#endif

import Prelude hiding (lookup)
import GHC.Conc(numCapabilities, forkIO)
import Control.Concurrent.MVar
import Control.Exception
import Dump
import Options.Applicative

threadLoop :: Int -> TVar Int -> IO()
threadLoop 0 c = return()
threadLoop i c = do
           atomically $ do t <- readTVar c; writeTVar c (t+1)
           threadLoop (i-1) c

mkThreads :: TVar Int -> Int -> Int -> IO [MVar ()]
mkThreads c 0 iters = return []
mkThreads c i iters = do
          mv <- newEmptyMVar
          forkIO (do threadLoop iters c; putMVar mv())
          mvs <- mkThreads c (i-1) iters
          return(mv:mvs)

join :: [MVar()] -> IO ()
join [] = return()
join (mv:mvs) = do
     takeMVar mv
     join mvs
     return()

parser :: Parser Int
parser = option auto (long "iters" <> short 'n' <> metavar "K" <> help "Number of iterations to perform") 

opts = info (helper  <*> parser) fullDesc


main = do
     opts <- execParser opts
     counter <- newTVarIO 0
     mvars <- mkThreads counter numCapabilities opts
     join mvars --wait for everyone to finish adding to list
     count <- readTVarIO counter
     putStrLn("Count is " ++ show count)
     return()
