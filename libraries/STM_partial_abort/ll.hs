{-# LANGUAGE CPP #-} 

--To build with full abort: ghc -threaded -DFULL_ABORT ll.hs
#ifdef FULL_ABORT
import GHC.Conc.Sync   --full abort STM
#else
import PASTM           --partial abort STM
#endif

import Prelude hiding (lookup)
import GHC.Conc(numCapabilities, forkIO)
import Control.Concurrent.MVar
import Control.Exception
import Dump

data STMList a = Head (TVar (STMList a))
               | Null
               | Node a (TVar (STMList a))

newList :: IO (TVar (STMList a))
newList = do
        nullPtr <- newTVarIO Null
        l <- newTVarIO (Head nullPtr)
        return(l)


initList :: Integer -> Integer -> IO (TVar (STMList Integer))
initList 0 j = do
         tv <- newTVarIO Null
         return(tv)
initList i j = do
     tail <- initList (i-1) (j+1)
     head <- newTVarIO (Node j tail)
     return(head)

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
          forkIO (do threadLoop l opList; remove l opList; putMVar mv())
          mvs <- mkThreads l (i-1)
          return(mv:mvs)

join :: [MVar()] -> IO ()
join [] = return()
join (mv:mvs) = do
     takeMVar mv
     join mvs
     return()

check :: TVar (STMList a) -> IO ()
check l = do
      raw <- readTVarIO l
      case raw of
           Head l' -> do
                rawL' <- readTVarIO l'
                case rawL' of
                     Null -> putStrLn "Invariant check passed!" >>= \ _ ->  return()
                     _ -> throw $ AssertionFailed "Non empty list after transactions"
           _ -> throw $ AssertionFailed "Headless list after transactions"

toList :: Show a => TVar (STMList a) -> IO [a]
toList l = do
       raw <- readTVarIO l
       case raw of
            Head l' -> toList l'
            Null -> return []
            Node hd tl -> do
                 tl' <- toList tl
                 return(hd : tl')

listLen :: TVar (STMList a) -> IO Int
listLen l = do
        raw <- readTVarIO l
        case raw of
             Head l -> listLen l
             Null -> return 0
             Node hd tl -> do
                  len <- listLen tl
                  return (len + 1)

main = do
     stmList <- newList
     mvars <- mkThreads stmList numCapabilities
     join mvars --wait for everyone to finish adding to list
     putStrLn "Threads are finished with their operations"
     check stmList `catch` \ msg -> do raw <- toList stmList; putStrLn (show (msg::AssertionFailed) ++ show raw)
     return()


