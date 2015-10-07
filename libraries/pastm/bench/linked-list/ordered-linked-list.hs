{-# LANGUAGE CPP, MagicHash #-} 
module LinkedList(newList, add, find, delete, deleteIndex, ListHandle, printStats, getSizeIO, addIO)
where

#ifdef STMHASKELL
import Control.STMHaskell.STM     --full abort STM haskell
#elif defined(FABORT)
import Control.NoRec.STM           --full abort STM (NoRec)
#elif defined(ORDERED)
import Control.Ordered.STM
#elif defined(PTL2)
import Control.PartialTL2.STM
#elif defined(CPSFULL)
import Control.NoRec.STM
#elif defined(CHUNKED)
import Control.Chunked.STM
#elif defined(FF)
import Control.FastForward.STM
#elif defined(PABORT)
import Control.NoRec.STM
#elif defined(TL2)
import Control.FullTL2.STM
#else
#error No STM Specified
#endif

-----------------------------------------------------------------------------
-- This file file annotates transactions for logging with the following mapping:
--   1# --> insert
--   2# --> find
--   3# --> delete
--   4# --> increment / decrement
-----------------------------------------------------------------------------

import Foreign.C.String
import Control.Common.STM
import Debug.Trace
import Data.Bits
import Data.Word
--import GHC.Prim(Int#)
import Data.IORef(IORef, readIORef, newIORef, atomicModifyIORef)

data List a = Node a (TVar (List a)) 
            | Null
            | Head (TVar (List a))
            
data ListHandle a = ListHandle (TVar (List a)) (IORef Int) 

getSizeIO (ListHandle hd size) = readIORef size

newList = do
        sizeIORef <- newIORef 0
        atomically $ do
                   l <- newTVar Null
                   head <- newTVar (Head l)
                   return (ListHandle head sizeIORef)

inc size n = do
    x <- readTVar size
    writeTVar size (x+n)

mkEvent :: Int -> Int -> Word64 -> Word64
mkEvent size item typ =  (shift (fromIntegral size) 34) .|. (shift (fromIntegral item) 4) .|. typ

--add :: Ord a => ListHandle a -> a -> IO()
add :: ListHandle Int -> Int -> IO()
add (ListHandle hd size) v = do s <- readIORef size;  atomically (addLoop hd) ; atomicModifyIORef size (\s -> (s+1, ()))
    where
    addLoop l = do
            raw <- readTVar l
            case raw of
                 Head n -> addLoop n
                 Null -> do
                      newNull <- newTVar Null
                      writeTVar l (Node v newNull)
                 Node v' n -> 
                      if v > v'
                      then addLoop n
                      else do
                           newNode <- newTVar (Node v' n)
                           writeTVar l (Node v newNode)

--find :: Eq a => ListHandle a -> a -> IO Bool
find :: ListHandle Int -> Int -> IO Bool
find (ListHandle hd size) v = do s <- readIORef size; atomically (findLoop hd)
     where
     findLoop l = do
              raw <- readTVar l
              case raw of
                   Head n -> findLoop n
                   Null -> return(False)
                   Node v' n ->
                              if v == v'
                              then return True
                              else findLoop n

delete :: Ord a => ListHandle a -> a -> IO Bool
delete (ListHandle hd size) v = do 
       res <- atomically (readTVar hd >>= \(Head n) -> deleteLoop n hd) ; 
       if res
       then atomicModifyIORef size (\s -> (s+1, res)) --atomically' (inc size (-1)) 4 >>= \_ -> return res
       else return res
       where
       deleteLoop l prev = do
                  raw <- readTVar l
                  case raw of
                       Null -> return False
                       Node v' n ->
                                  if v /= v'
                                  then deleteLoop n l
                                  else do
                                       rawPrev <- readTVar prev
                                       case rawPrev of
                                            Head n -> writeTVar prev (Head n) >>= \_ -> return True
                                            Node v'' n' -> writeTVar prev (Node v'' n) >>= \_ -> return True
                                                     
next (Head n) = n
next (Node _ n) = n

deleteIndex (ListHandle hd size) v = do
            s <- readIORef size
            res <- atomically (readTVar hd >>= \(Head n) -> deleteLoop n hd v)
            if res 
            then atomicModifyIORef size (\s -> (s+1, res)) --atomically' (inc size (-1)) 4 >>= \_ -> return res
            else return res
            where
            deleteLoop l prev 0 = do
                       raw <- readTVar l 
                       prevRaw <- readTVar prev
                       case prevRaw of
                            Head n -> writeTVar prev (Head (next raw)) >>= \_ -> return True
                            Node v' n -> writeTVar prev (Node v' (next raw)) >>= \_ -> return True
            deleteLoop l prev i = do
                       raw <- readTVar l
                       case raw of
                            Head n -> deleteLoop n l (i-1)
                            Node _ n -> deleteLoop n l (i-1)
                            Null -> return False
                       

addIO :: Ord a => ListHandle a -> a -> IO()
addIO (ListHandle hd size) v = do addLoop hd; atomicModifyIORef size (\s -> (s+1, ())) --s <- readIORef size; writeIORef size (s+1)
    where
    addLoop l = do
            raw <- readTVarIO l
            case raw of
                 Head n -> addLoop n
                 Null -> do
                      newNull <- newTVarIO Null 
                      writeTVarIO l (Node v newNull)
                 Node v' n -> 
                      if v > v'
                      then addLoop n
                      else do
                           newNode <- newTVarIO (Node v' n)
                           writeTVarIO l (Node v newNode)
