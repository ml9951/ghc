import PASTM
import Prelude hiding (lookup)
import GHC.Conc(numCapabilities, forkIO)
import Control.Concurrent.MVar

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

delete :: Eq a => TVar (STMList a) -> a -> STM()
delete trailer x = (do ptr <- next trailer; loop ptr trailer) where
       loop ptr trailer =
            readTVar ptr >>= \raw ->
            case raw of
                 Head _ -> error("delete: Impossible")
                 Null -> return()
                 Node hd tl ->
                      if x == hd
                      then readTVar trailer >>= \rawTrailer ->
                           case rawTrailer of Head t -> writeTVar trailer (Head tl) >>= \_ -> return()
                                              Node hd tl -> readTVar tl >>= \rawTL -> writeTVar trailer rawTL >>= \_ -> return()
                                              _ -> error "delete: loop: Impossible"
                      else loop tl ptr

threadLoop :: Ord a => MVar() -> TVar (STMList a) -> [a] -> IO()
threadLoop mv l [] = do putMVar mv (); return()
threadLoop mv l (hd:tl) = do
           atomically $ insert l hd
           res <- atomically $ lookup l hd
           if res == True
           then do 
                atomically $ delete l hd
                threadLoop mv l tl
                return()
           else error ("Lookup invariant failed!")
         
mkThreads :: TVar (STMList Int) -> Int -> IO [MVar ()]
mkThreads l 0 = return []
mkThreads l i = do
          mv <- newEmptyMVar
          forkIO (threadLoop mv l [(0::Int)..500])
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
                     _ -> error("Non empty list after transactions")
           _ -> error ("Headless list after transactions")

main = do
     stmList <- newList
     mvars <- mkThreads stmList numCapabilities
     join mvars
     check stmList
     return()


