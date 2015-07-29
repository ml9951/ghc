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
import System.Environment

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

data Op = Lookup | Insert | Delete
  
instance Show Op where
         show Lookup = "1"
         show Insert = "2"
         show Delete = "3"


type Stats = [(Op, Word, Int, Double)]



threadLoop :: ListHandle Word -> Opts -> Int -> [Word] -> Stats -> IO Stats
threadLoop l opts 0 rands times = return times
threadLoop l opts i (r:rands) times = 
           let sum = reads opts + writes opts + deletes opts
               prob = r `mod` fromIntegral sum
           in 
              if prob < fromIntegral (reads opts)
              then do start <- getTime; find l r; end <- getTime; s <- getSizeIO l; threadLoop l opts (i-1) rands ((Lookup, r, s, end-start::Double):times)
              else if prob < fromIntegral (reads opts + writes opts)
                   then do start <- getTime; add l r; end <- getTime; s <- getSizeIO l; threadLoop l opts (i-1) rands ((Insert, r, s, end-start::Double):times)
                   else do 
                        size <- getSize l
                        start <- getTime
                        delete l (r `mod` fromIntegral size)
                        end <- getTime
                        threadLoop l opts (i-1) rands ((Delete, r, size, end-start::Double):times)

mkThreads 0 l opts = return[]
mkThreads i l opts = do
          mv <- newEmptyMVar
          forkOn (i-1) $ do let rs :: [Word]
                                rs = randoms (mkStdGen i) 
                            times <- threadLoop l opts (iters opts) rs []
                            putMVar mv times
          mvs <- mkThreads (i-1) l opts
          return $ mv : mvs

join [] = return []
join (mv:mvs) = do
     times <- takeMVar mv
     moreTimes <- join mvs
     return (times : moreTimes)

formatTimes [] = ""
formatTimes ((typ, n, s, t):rest) = show typ ++ ", " ++ show n ++ ", " ++ show s ++ ", " ++ show (t * 1000) ++ "\n" ++ formatTimes rest
           

main = do
     name <- getProgName
     opts <- execParser opts
     print opts
     l <- newList

     mapM_ (\x -> add l (x::Word)) (take (initSize opts) (randoms (mkStdGen $ numCapabilities+1)))
     s <- getSizeIO l
     putStrLn("Starting with list at size " ++ show s)

     start <- getTime
     mvars <- mkThreads (threads opts) l opts
     times <- join mvars
     end <- getTime
     printf "Time = %0.3f\n" (end - start :: Double)
     printStats
     let strings = foldl (++) "" (map formatTimes times)
     writeFile (name ++ "Times.txt") strings
     s <- getSizeIO l
     putStrLn ("List size is " ++ show s)
     return()

foreign import ccall unsafe "hs_gettime" getTime :: IO Double

