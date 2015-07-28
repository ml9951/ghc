{-# LANGUAGE CPP #-} 
module LinkedList(newList, add, find, delete, deleteIndex, ListHandle, getSize, printStats, getSizeIO)
where

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

data List a = Node {val :: a, next :: (TVar (List a))}
            | Null
            | Head {next :: (TVar (List a))}

data ListHandle a = ListHandle{hd :: TVar (List a), size :: TVar Int}

getSize (ListHandle{hd=hd,size=size}) = atomically $ readTVar size

getSizeIO (ListHandle{hd=hd,size=size}) = readTVarIO size

newList = atomically $ do
        l <- newTVar Null
        count <- newTVar 0
        return (ListHandle{hd=l, size=count})

inc size n = do
    x <- readTVar size
    writeTVar size (x+n)

add :: Ord a => ListHandle a -> a -> IO()
add (ListHandle{hd=hd, size=size}) v = do atomically (addLoop hd); atomically (inc size 1)
    where
    addLoop l = do
            raw <- readTVar l
            case raw of
                 Head n -> addLoop n
                 Null -> do
                      newNull <- newTVar Null
                      writeTVar l (Node{val=v,next=newNull})
                 Node v' n -> 
                      if v > v'
                      then addLoop n
                      else do
                           newNode <- newTVar (Node v' n)
                           writeTVar l (Node {val=v, next=newNode})

find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle{hd=hd,size=size}) v = atomically (findLoop hd)
     where
     findLoop l = do
              raw <- readTVar l
              case raw of
                   Head n -> findLoop n
                   Null -> return(False)
                   Node{val=v',next=n} ->
                                      if v == v'
                                      then return True
                                      else findLoop n

delete :: Ord a => ListHandle a -> a -> IO Bool
delete (ListHandle{hd=hd,size=size}) v = do 
       res <- atomically (readTVar hd >>= \raw -> deleteLoop (next raw) hd); 
       if res
       then atomically (inc size (-1)) >>= \_ -> return res
       else return res
       where
       deleteLoop l prev = do
                  raw <- readTVar l
                  case raw of
                       Null -> return False
                       Node{val=v',next=n} ->
                                           if v /= v'
                                           then deleteLoop n l
                                           else do
                                                rawPrev <- readTVar prev
                                                case rawPrev of
                                                     Head n -> writeTVar prev (Head n) >>= \_ -> return True
                                                     Node{val=v'',next=n'} -> writeTVar prev (Node{val=v'',next=n}) >>= \_ -> return True
                                                     

deleteIndex (ListHandle{hd=hd,size=size}) v = do
            res <- atomically (readTVar hd >>= \raw -> deleteLoop (next raw) hd v)      
            if res 
            then atomically (inc size (-1)) >>= \_ -> return res
            else return res
            where
            deleteLoop l prev 0 = do
                       raw <- readTVar l 
                       prevRaw <- readTVar prev
                       case prevRaw of
                            Head n -> writeTVar prev (Head (next raw)) >>= \_ -> return True
                            Node{val=v',next=n} -> writeTVar prev (Node{val=v',next=next raw}) >>= \_ -> return True
            deleteLoop l prev i = do
                       raw <- readTVar l
                       case raw of
                            Head n -> deleteLoop n l (i-1)
                            Node{next=n} -> deleteLoop n l (i-1)
                            Null -> return False
                       

