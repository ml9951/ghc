{-# LANGUAGE CPP, ForeignFunctionInterface #-} 

module StraightForwardSTM(newList, addToTail, find, delete, List, printStats)
where

#ifdef STMHASKELL
import Control.STMHaskell.STM     --full abort STM (STM Haskell)
#elif defined(FABORT)
import Control.Full.STM           --full abort STM (NoRec)
#elif defined(ORDERED)
import Control.Ordered.STM
#elif defined(CPSFULL)
import Control.CPSFull.STM
#else
import Control.Partial.STM
--import Control.Concurrent.PASTM.Core
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
delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle {headList = ptrPtr})  i =
       atomically (do startptr <- readTVar ptrPtr
                      delete2 startptr i)
       where
       delete2 :: Eq a => TVar (List a) -> a -> STM Bool
       delete2 prevPtr i = do
               prevNode <- readTVar prevPtr
               let curNodePtr = next prevNode
               curNode <- readTVar curNodePtr
               case curNode of
                    Null -> return False
                    Node {val = curval, next = nextNode} ->
                         if (curval /= i)
                         then delete2 curNodePtr i -- keep searching
                         else case prevNode of
                            Head {} -> do
                                 writeTVar prevPtr (Head {next = nextNode})
                                 return True
                            Node {} -> do
                                 writeTVar prevPtr (Node {val = val prevNode,
                                                          next = nextNode})
                                 return True                          






