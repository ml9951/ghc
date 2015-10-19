{-# LANGUAGE CPP, ForeignFunctionInterface #-} 

module StraightForwardSTM(newList, addToTail, find, delete, List, printStats, whichSTM)
where

#ifdef STMHASKELL
import Control.STMHaskell.STM     --full abort STM (STM Haskell)
#elif defined(FABORT)
import Control.Full.STM           --full abort STM (NoRec)
#elif defined(ORDERED)
import Control.Ordered.STM
#elif defined(CPSFULL)
import Control.CPSFull.STM
#elif defined(PABORT)
import Control.Partial.STM
#elif defined(PTL2)
import Control.PartialTL2.STM
#elif defined(CHUNKED)
import Control.Chunked.STM
#elif defined(PRTS)
import Control.Concurrent.PASTM.Core
#elif defined(CHUNKED_TL2)
import Control.ChunkedTL2.STM
#else
#error NO STM SPECIFIED
#endif

import Dump

#ifdef STMHASKELL
whichSTM = "STM Haskell"
#elif defined(FABORT)
whichSTM = "Full Abort NoRec"
#elif defined(ORDERED)
whichSTM = "Ordered NoRec"
#elif defined(CPSFULL)
whichSTM = "CPS Converted Full Abort NoRec"
#elif defined(PABORT)
whichSTM = "Partial Abort NoRec"
#elif defined(PTL2)
whichSTM = "Partial Abort TL2"
#elif defined(CHUNKED)
whichSTM = "Chunked (Ordered) NoRec"
#elif defined(PRTS)
whichSTM = "Partial Abort NoRec (RTS version)"
#elif defined(CHUNKED_TL2)
whichSTM = "Chunked TL2"
#else
#error NO STM SPECIFIED
#endif

data List a = Node { val :: a,
                     next :: TVar (List a) }
            | Null
            | Head { next :: TVar (List a) }
data ListHandle a
     = ListHandle { headList :: TVar (TVar (List a)),
                    tailList :: TVar (TVar (List a)) }
newList :: IO (ListHandle a)
newList =
 do null <- atomically (newTVar Null)
    hd <- atomically (newTVar (Head {next = null }))
    hdPtr <- atomically (newTVar hd)
    tailPtr <- atomically (newTVar null)
    return (ListHandle {headList = hdPtr,
                        tailList = tailPtr})
addToTail :: ListHandle a -> a -> IO ()
addToTail (ListHandle {tailList = tailPtrPtr}) x = do
  tPtr <- atomically ( do
            null <- newTVar Null
            tailPtr <- readTVar tailPtrPtr
            writeTVar tailPtr
                      (Node {val = x, next = null})
            writeTVar tailPtrPtr null
            return()
           )
  return tPtr

find :: Eq a => ListHandle a -> a -> IO Bool 
find (ListHandle {headList = ptrPtr}) i =
  atomically (
       do ptr <- readTVar ptrPtr
          Head {next = startptr} <- readTVar ptr
          find2 startptr i)
   where
    find2 :: Eq a => TVar (List a) -> a -> STM Bool
    find2 curNodePtr i = do
      curNode <- readTVar curNodePtr
      case curNode of
        Null -> return False
        Node {val = curval, next = curnext} ->
           if (curval == i) then return True
           else find2 curnext i

getNext (Node{next=n}) = return n
getNext (Head{next=n}) = return n

delete (ListHandle{headList=ptrPtr}) i =
        atomically(do startPtr <- readTVar ptrPtr
                      raw <- readTVar startPtr
                      n <- getNext raw
                      delete2 startPtr n)
        where
        delete2 prevPtr currentPtr = do
                curNode <- readTVar currentPtr
                case curNode of
                     Null -> return False
                     Node{val = curval, next = nextNode} ->
                              if curval /= i
                              then delete2 currentPtr nextNode
                              else do 
                                   prevNode <- readTVar prevPtr
                                   case prevNode of
                                        Head{} -> do
                                               writeTVar prevPtr (Head{next=nextNode})
                                               return True
                                        Node{} -> do
                                               writeTVar prevPtr (Node{val=val prevNode, next=nextNode})
                                               return True
