{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (reads)
import GHC.Conc(numCapabilities)
import Options.Applicative
import LinkedList
import Control.Monad.Trans.State
import System.Random
import Control.Concurrent.MVar
import Control.Concurrent(forkOn)
import Text.Printf

data Opts = Opts
     { iters     :: Int
     , threads   :: Int
     , maxVal    :: Int
     , initSize  :: Int
     , reads     :: Int
     , writes    :: Int
     , deletes   :: Int
     } deriving Show

{-
parser :: String -> Opts
parser prog = Opts
       { iters      = 3000
                   &= help "Number of operations performed by each thread"
       , threads    = getNumCapabilities
                   &= help "Number of threads"
       , maxVal     = 100000
                   &= help "Largest value to be added to the list"
       , initSize   = 4000
                   &= help "Initial size of the linked list"
       , reads      = 2
                   &= help "Read proportion"
       , writes     = 4
                   &= help "Write proportion"
       , deletes    = 1
                   &= help "Delete proportion"
       }
       &= program prog
-}
parser :: Parser Opts
parser = Opts
      <$> option auto (short 'i' <> help "Number of operations performed by each thread" <> value 3000)
      <*> option auto (short 't' <> help "Number of threads" <> value numCapabilities)
      <*> option auto (long "maxVal" <> help "Max value to be added to the list" <> value 10000000)
      <*> option auto (long "initSize" <> help "Initial size of the linked list" <> value 4000)
      <*> option auto (long "reads" <> help "Read proportion" <> value 2)
      <*> option auto (long "writes" <> help "Write proportion" <> value 4)
      <*> option auto (long "deletes" <> help "Delete proportion" <> value 1)

opts = info (helper  <*> parser) fullDesc



threadLoop l opts 0 rands = return()
threadLoop l opts i (r:rands)= 
           let sum = reads opts + writes opts + deletes opts
               prob = r `mod` sum
           in 
              if prob < reads opts
              then find l r >>= \_ -> threadLoop l opts (i-1) rands
              else if prob < reads opts + writes opts
                   then add l r >>= \_ -> threadLoop l opts (i-1) rands
                   else do 
                        size <- getSize l
                        delete l (r `mod` size)
                        threadLoop l opts (i-1) rands

mkThreads 0 l opts = return[]
mkThreads i l opts = do
          mv <- newEmptyMVar
          forkOn (i-1) $ do threadLoop l opts (iters opts) (randoms (mkStdGen i)); putMVar mv()
          mvs <- mkThreads (i-1) l opts
          return $ mv : mvs

join :: [MVar()] -> IO ()
join [] = return()
join (mv:mvs) = do
     takeMVar mv
     join mvs


main = do
     opts <- execParser opts
     print opts
     l <- newList
     start <- getTime
     mvars <- mkThreads (threads opts) l opts
     join mvars
     end <- getTime
     printf "Time = %0.3f\n" (end - start :: Double)
     printStats
     return()

foreign import ccall unsafe "hs_gettime" getTime :: IO Double

