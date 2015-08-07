{-# LANGUAGE CPP, ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnliftedFFITypes #-} 

#ifdef STMHASKELL
import Control.STMHaskell.STM     --full abort STM (STM Haskell)
#elif defined(FABORT)
import Control.Full.STM           --full abort STM (NoRec)
#elif defined(ORDERED)
import Control.Ordered.STM
#elif defined(CPSFULL)
import Control.CPSFull.STM
#elif defined(PTL2)
import Control.PartialTL2.STM
#elif defined(TL2)
import Control.FullTL2.STM
#elif defined(CHUNKED)
import Control.Chunked.STM
#elif defined(PABORT)
import Control.Partial.STM
#elif defined(FF)
import Control.Ordered.STM
#else
#error No STM Specified
#endif

import Prelude hiding (lookup)
import GHC.Conc(numCapabilities, forkOn)
import Control.Concurrent.MVar
import Control.Exception
import Dump
import Data.Map(Map, empty)
import Text.Printf
import Control.Monad(foldM_)
import GHC.Prim(Any, unsafeCoerce#, Int#)

data STMList a = Head (TVar (STMList a))
               | Null
               | Node a (TVar (STMList a))

newList :: IO (TVar (STMList a))
newList = do
        nullPtr <- newTVarIO Null
        l <- newTVarIO (Head nullPtr)
        return(l)

lookup :: (Eq a, Show a) => TVar (STMList a) -> a -> STM Bool
lookup l x = do
       y <- readTVar l
       case y of
            Head t -> lookup t x
            Null -> return(False)
            Node hd tl -> 
                     if x == hd
                     then return(True)
                     else lookup tl x

insert :: TVar (STMList a) -> a -> IO()
insert l x = do
       raw <- readTVarIO l        
       case raw of
            Head t -> do
                 newNode <- newTVarIO (Node x t)
                 writeTVarIO l (Head newNode)
            

loop 0 l = return()
loop i l = atomically(lookup l 100000000) >>= \_ -> loop (i-1) l

main = do
     stmList <- newList
     foldM_ (\b -> \a -> insert stmList a) () [10000, 9999..0]
     putStrLn "Done initializing"
     start <- getTime
     loop 5000 stmList
     end <- getTime
     printf "Time = %0.3f\n" (end - start :: Double)
     printStats
     return()

foreign import ccall unsafe "hs_gettime" getTime :: IO Double
