

import Prelude hiding (reads)
import GHC.Conc(numCapabilities)
import Options.Applicative
import LinkedList
import Control.Concurrent.MVar
import Control.Concurrent(forkOn)
import Text.Printf
import Control.Common.STM
import System.Random.PCG.Fast.Pure
import Dump

data Opts = Opts
     { iters     :: Int
     , threads   :: Int
     , maxVal    :: Int
     , initSize  :: Int
     , reads     :: Int
     , writes    :: Int
     , deletes   :: Int
     } deriving Show

parser :: Parser Opts
parser = Opts
      <$> option auto (short 'i' <> help "Number of operations performed by each thread" <> value 3000)
      <*> option auto (short 't' <> help "Number of threads" <> value numCapabilities)
      <*> option auto (long "maxVal" <> help "Max value to be added to the list" <> value 10000000)
      <*> option auto (long "initSize" <> help "Initial size of the linked list" <> value 3000)
      <*> option auto (long "reads" <> help "Read proportion" <> value 2)
      <*> option auto (long "writes" <> help "Write proportion" <> value 4)
      <*> option auto (long "deletes" <> help "Delete proportion" <> value 1)

opts = info (helper  <*> parser) fullDesc

threadLoop :: ListHandle Int -> Opts -> Int -> GenIO -> IO()
threadLoop l opts 0 g = return()
threadLoop l opts i g = do
           r <- uniformR (0, maxVal opts) g
           let sum = reads opts + writes opts + deletes opts
               prob = r `mod` sum
           if prob < fromIntegral (reads opts)
           then do find l r; threadLoop l opts (i-1) g
           else if prob < fromIntegral (reads opts + writes opts)
                then do add l r; threadLoop l opts (i-1) g
                else do 
                     size <- getSizeIO l
                     deleteIndex l (r `mod` fromIntegral size)
                     threadLoop l opts (i-1) g

mkThreads 0 l opts = return[]
mkThreads i l opts = do
          mv <- newEmptyMVar
          g <- initialize $ fromIntegral i
          forkOn (i-1) $ do threadLoop l opts (iters opts) g; putMVar mv ()
          mvs <- mkThreads (i-1) l opts
          return $ mv : mvs

join [] = return []
join (mv:mvs) = do
     times <- takeMVar mv
     moreTimes <- join mvs
     return (times : moreTimes)

formatTimes [] = ""
formatTimes ((typ, n, s, t):rest) = show typ ++ ", " ++ show n ++ ", " ++ show s ++ ", " ++ show (t * 1000) ++ "\n" ++ formatTimes rest
           
initList :: Int -> ListHandle Int -> GenIO -> Int -> IO()
initList 0 l g maxVal = return()
initList i l g maxVal = do
           r <- uniformR(0, maxVal) g
           addIO l r
           initList (i-1) l g maxVal

main = do
     opts <- execParser opts
     print opts
     l <- newList
     g <- (initialize 0)
     initList (initSize opts) l g (maxVal opts)
    -- mapM_ (\x -> addIO l (x::Word)) (take (initSize opts) (randoms (mkStdGen $ numCapabilities+1)))
     s <- getSizeIO l
     putStrLn("Starting with list at size " ++ show s)

     start <- getTime
     mvars <- mkThreads (threads opts) l opts
     join mvars
     end <- getTime
     printf "Time = %0.3f\n" (end - start :: Double)
     printStats
     s <- getSizeIO l
     putStrLn ("List size is " ++ show s)
     printSTMProfile
     return()

foreign import ccall unsafe "hs_gettime" getTime :: IO Double

