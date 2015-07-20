import PASTM
import Prelude hiding (lookup)
import GHC.IO
import Debug.Trace

data STMList a = Head (TVar (STMList a))
               | Null
               | Node a (TVar (STMList a))


initList :: Integer -> Integer -> IO (TVar (STMList Integer))
initList 0 j = do
         tv <- newTVarIO Null
         return(tv)
initList i j = do
     tail <- initList (i-1) (j+1)
     head <- newTVarIO (Node j tail)
     return(head)

toList :: TVar (STMList Integer) -> IO ([Integer])
toList l = do
       x <- readTVarIO l
       case x of
            Head t -> do
                 x <- toList t
                 return(x)
            Null -> return([])
            Node hd tl -> do
                 tail <- toList tl
                 return(hd : tail)

lookup :: Ord a => a -> TVar (STMList a) -> STM Bool
lookup x l = do
       y <- readTVar l
       case y of
            Head t -> lookup x t
            Null -> return(False)
            Node hd tl -> 
                     if x == hd
                     then return(True)
                     else lookup x tl


insert :: Ord a => Show a => TVar (STMList a) -> a -> STM ()
insert l x = do
       raw <- readTVar l        
       case raw of
            Head t -> insert t x
            Null -> do
                 traceM ("Writing " ++ show x ++ " at end of list")
                 newTail <- newTVar Null
                 writeTVar l (Node x newTail)
                 return()
            Node hd tl -> do
                 if x < hd
                 then do
                      traceM ("Writing " ++ show x ++ " after node " ++ show hd)
                      newNode <- newTVar (Node hd tl)
                      writeTVar l (Node x newNode)
                      return ()
                 else trace ("Read: " ++ show hd) $ insert tl x
                     
printList l = do
          list <- toList l
          print("List = " ++ show list)

main = do
     stmList <- initList 50 0
     printList stmList
     atomically $ insert stmList 100
     printList stmList
     return()

