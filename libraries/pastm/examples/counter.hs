{-# LANGUAGE CPP, BangPatterns #-} 

#ifdef STMHASKELL
import Control.STMHaskell.STM     --full abort STM haskell
#elif defined(FABORT)
import Control.Full.STM           --full abort STM (NoRec)
#elif defined(ORDERED)
import Control.Ordered.STM
#elif defined(TL2)
import Control.PartialTL2.STM
#else
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
           atomically $ do 
                      t <- readTVar c; 
                      let !n = t+1; 
                      writeTVar c n
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
parser = option auto (long "iters" <> short 'n' <> metavar "K" <> help "Number of iterations to perform" <> value 5000) 

opts = info (helper  <*> parser) fullDesc


main = do
     opts <- execParser opts
     counter <- newTVarIO 0
     mvars <- mkThreads counter numCapabilities opts
     join mvars --wait for everyone to finish adding to list
     count <- readTVarIO counter
     putStrLn("Count is " ++ show count)
     return()

