{-# LANGUAGE CPP, ForeignFunctionInterface #-} 

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


data STMList a = Head (TVar (STMList a))
               | Null
               | Node a (TVar (STMList a))

newList :: IO (TVar (STMList a))
newList = do
        nullPtr <- newTVarIO Null
        l <- newTVarIO (Head nullPtr)
        return(l)

lookup :: Eq a => TVar (STMList a) -> a -> STM Bool
lookup l x = do
       y <- readTVar l
       case y of
            Head t -> lookup t x
            Null -> return(False)
            Node hd tl -> 
                     if x == hd
                     then return(True)
                     else lookup tl x

insert :: Ord a => TVar (STMList a) -> a -> STM()
insert l x = do
       raw <- readTVar l        
       case raw of
            Head t -> insert t x
            Null -> do
                 newTail <- newTVar Null
                 writeTVar l (Node x newTail)
                 return()
            Node hd tl -> 
                 if x < hd
                 then do
                      newNode <- newTVar (Node hd tl)
                      writeTVar l (Node x newNode)
                      return()
                 else insert tl x

next :: TVar (STMList a) -> STM (TVar (STMList a))
next l = do
     raw <- readTVar l
     case raw of
          Head t -> return(t)
          _ -> error ("Headless list")

delete :: Eq a => TVar (STMList a) -> a -> STM Bool
delete trailer x = (do ptr <- next trailer; loop ptr trailer) where
       loop ptr trailer =
            readTVar ptr >>= \raw ->
            case raw of
                 Head _ -> error("delete: Impossible")
                 Null -> return False
                 Node hd tl ->
                      if x == hd
                      then readTVar trailer >>= \rawTrailer ->
                           case rawTrailer of Head t -> writeTVar trailer (Head tl) >>= \_ -> return(True)
                                              Node hd' tl' -> writeTVar trailer (Node hd' tl) >>= \_ -> return True
                                              _ -> error "delete: loop: Impossible"
                      else loop tl ptr

threadLoop :: Ord a => TVar (STMList a) -> [a] -> IO()
threadLoop l [] = return()
threadLoop l (hd:tl) = do
           atomically $ insert l hd
           res <- atomically $ lookup l hd
           if res == True
           then threadLoop l tl
           else putStrLn "Lookup invariant failed!" >>= \_ -> threadLoop l tl
         
remove :: Show a => Ord a => TVar (STMList a) -> [a] -> IO()
remove l [] = return()
remove l (hd:tl) = do
       removed <- atomically $ delete l hd 
       if removed == True
       then remove l tl >>= \_ -> return()
       else do
            putStrLn("Error, tried to remove element " ++ show hd ++ ", but it was not found in list")
            remove l tl 
            return()
       
mkThreads :: TVar (STMList Int) -> Int -> IO [MVar ()]
mkThreads l 0 = return []
mkThreads l i = do
          mv <- newEmptyMVar
          opList <- return [0..500]
          forkOn (i-1) (do threadLoop l opList; remove l opList; putMVar mv ())
          mvs <- mkThreads l (i-1)
          return(mv:mvs)

join :: [MVar()] -> IO ()
join [] = return()
join (mv:mvs) = do
     takeMVar mv
     join mvs

check :: TVar (STMList a) -> IO ()
check l = do
      raw <- readTVarIO l
      case raw of
           Head l' -> do
                rawL' <- readTVarIO l'
                case rawL' of
                     Null -> putStrLn "Success: Invariant check passed!" >>= \ _ ->  return()
                     _ -> throw $ AssertionFailed "Error: Non empty list after transactions"
           _ -> throw $ AssertionFailed "Error: Headless list after transactions"

toList :: Show a => TVar (STMList a) -> IO [a]
toList l = do
       raw <- readTVarIO l
       case raw of
            Head l' -> toList l'
            Null -> return []
            Node hd tl -> do
                 tl' <- toList tl
                 return(hd : tl')

main = do
     stmList <- newList
     start <- getTime
     mvars <- mkThreads stmList numCapabilities
     join mvars
     end <- getTime
     printf "Time = %0.3f\n" (end - start :: Double)
     printStats
     check stmList `catch` \ msg -> do raw <- toList stmList; putStrLn (show (msg::AssertionFailed) ++ show raw)
     return()


foreign import ccall unsafe "hs_gettime" getTime :: IO Double