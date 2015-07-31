{-# LANGUAGE CPP, ForeignFunctionInterface #-} 

module DissectedSTM(newList, addToTail, find, delete, List, printStats, whichSTM)
where

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
#elif defined(PABORT)
import Control.Partial.STM
#else
#error No STM Specified
#endif

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
#else
#error NO STM SPECIFIED
#endif

data List a = Node { val :: a
                   , next :: TVar (List a) }
            | DelNode { next :: TVar (List a) }
            | Null
            | Head { next :: TVar (List a) }

data ListHandle a
     = ListHandle { headList :: TVar (TVar (List a)),
                    tailList :: TVar (TVar (List a)) }

find :: Eq a => ListHandle a -> a -> IO Bool
find lh x = searchAndExecute lh x $ \_ _ -> return (return True)

delete :: Eq a => ListHandle a -> a -> IO Bool
delete lh x = searchAndExecute lh x $
               \curPtr curNode ->  do
                       writeTVar curPtr (DelNode{next=next curNode})
                       return(return True)

newList :: IO (ListHandle a)
newList = do 
        null <- atomically (newTVar Null)
        hd <- atomically (newTVar (Head {next = null }))
        hdPtr <- atomically (newTVar hd)
        tailPtr <- atomically (newTVar null)
        return (ListHandle {headList = hdPtr,
                            tailList = tailPtr})

addToTail :: ListHandle a -> a -> IO ()
addToTail (ListHandle {tailList = tailPtrPtr}) x = do
          atomically ( do
                     null <- newTVar Null
                     tailPtr <- readTVar tailPtrPtr
                     writeTVar tailPtr (Node {val = x, next = null})
                     writeTVar tailPtrPtr null
                     return()
                     )
          return()

searchAndExecute
    :: Eq a
    => ListHandle a
    -> a
    -> (TVar (List a)
       -> List a
       -> STM (IO Bool))
    -> IO Bool
searchAndExecute (ListHandle { headList = head }) x apply =
    do startPtr <- atomically (readTVar head)
       go startPtr
       where
       go prevPtr = loopSTM $ do
          prevNode <- readTVar prevPtr
          -- head/node/delnode all have next
          let curPtr = next prevNode
          curNode <- readTVar curPtr
          case curNode of
               Node {val = y, next = nextNode } ->
                    if x == y
                    then apply curPtr curNode
                    else return (go curPtr)
               Null ->  -- reached end of list
                    return (return False)
               DelNode { next = nextNode } ->
                       -- delete curNode by setting the next
                       -- of prevNode to next of curNode
                       case prevNode of
                            Node {} -> do
                                 writeTVar prevPtr
                                           (Node {val = val prevNode,
                                                  next = nextNode})
                                 return (go prevPtr)
                            Head {} -> do
                                 writeTVar prevPtr (Head {next = nextNode})
                                 return (go prevPtr)
                            DelNode {} ->
                                    -- if parent deleted simply move ahead
                                    return (go curPtr)

loopSTM :: STM (IO a) -> IO a
loopSTM stm = do
        action <- atomically stm
        action