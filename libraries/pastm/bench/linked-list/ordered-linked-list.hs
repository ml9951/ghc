{-# LANGUAGE CPP #-} 
module LinkedList(newList, add, find, delete, deleteIndex, ListHandle, getSize, printStats, getSizeIO, addIO)
where

#ifdef STMHASKELL
import Control.STMHaskell.STM     --full abort STM haskell
#elif defined(FABORT)
import Control.Full.STM           --full abort STM (NoRec)
#elif defined(ORDERED)
import Control.Ordered.STM
#elif defined(PTL2)
import Control.PartialTL2.STM
#elif defined(CPSFULL)
import Control.CPSFull.STM
#elif defined(CHUNKED)
import Control.Chunked.STM
#elif defined(FF)
import Control.FastForward.STM
#elif defined(PABORTNOFORALL)
import Control.PartialNoForall.STM
#else
import Control.Partial.STM
#endif

import Control.Common.STM
import Dump

data List a = Node a (TVar (List a)) --{val :: a, next :: (TVar (List a))}
            | Null
            | Head (TVar (List a)) --{next :: (TVar (List a))}

data ListHandle a = ListHandle (TVar (List a)) (TVar Int) --{hd :: TVar (List a), size :: TVar Int}

getSize (ListHandle hd size) = atomically $ readTVar size

getSizeIO (ListHandle hd size) = readTVarIO size

newList = atomically $ do
        l <- newTVar Null
        head <- newTVar (Head l)
        count <- newTVar 0
        return (ListHandle head count)

inc size n = do
    x <- readTVar size
    writeTVar size (x+n)

add :: Ord a => ListHandle a -> a -> IO()
add (ListHandle hd size) v = do atomically (addLoop hd); atomically (inc size 1)
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

find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle hd size) v = atomically (findLoop hd)
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
       res <- atomically (readTVar hd >>= \(Head n) -> deleteLoop n hd); 
       if res
       then atomically (inc size (-1)) >>= \_ -> return res
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
            res <- atomically (readTVar hd >>= \(Head n) -> deleteLoop n hd v)      
            if res 
            then atomically (inc size (-1)) >>= \_ -> return res
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
addIO (ListHandle hd size) v = do addLoop hd; s <- readTVarIO size; writeTVarIO size (s+1)
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
