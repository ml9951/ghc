module CASLL(List, find, delete)
where

import Data.IORef

data List a = Node { val :: a
                   , next :: IORef (List a) }
            | DelNode { next :: IORef (List a) }
            | Null
            | Head { next :: IORef (List a) }
            deriving Eq

data ListHandle a
     = ListHandle { headList :: IORef (IORef (List a)),
                    tailList :: IORef (IORef (List a)) }

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORef ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))


-- we create a new list
newList :: IO (ListHandle a)
newList = 
   do nullPtr <- newIORef Null
      hd <- newIORef (Head {next = nullPtr })
      hdPtr <- newIORef hd
      tailPtr <- newIORef nullPtr
      return (ListHandle {headList = hdPtr, tailList = tailPtr})


-- we add a new node, by overwriting the null tail node
-- we only need to adjust tailList but not headList because
-- of the static Head
-- we return the location of the newly added node
addToTail :: Eq a => ListHandle a -> a -> IO (IORef (List a))
addToTail handle@(ListHandle {tailList = tailPtrPtr}) x = do 
          nullPtr <- newIORef Null
          tailPtr <- readIORef tailPtrPtr
          b <- atomCAS tailPtr Null (Node{val = x, next = nullPtr})
          if b
          then return tailPtr
          else addToTail handle x


find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle { headList = head }) x = do
     startPtr <- readIORef head
     go startPtr
        where 
        go prevPtr = do
           prevNode <- readIORef prevPtr
           let curPtr = next prevNode
           curNode <- readIORef curPtr
           case curNode of
                Node{val = y, next = nextNode} ->
                         if x == y 
                         then return True
                         else go curPtr
                Null -> return False
                DelNode {next = nextNode} ->
                        case prevNode of
                             Node{} -> do
                                    b <- atomCAS prevPtr prevNode (Node {val=val prevNode, next = nextNode})
                                    if b
                                    then go prevPtr
                                    else go curPtr
                             Head{} -> do
                                    b <- atomCAS prevPtr prevNode (Head{next=nextNode})
                                    if b
                                    then go prevPtr
                                    else go curPtr
                             DelNode {} -> go curPtr
                             
delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle { headList = head }) x = do
       startPtr <- readIORef head
       go startPtr
          where
          go prevPtr = do
             prevNode <- readIORef prevPtr
             let curPtr = next prevNode
             curNode <- readIORef curPtr
             case curNode of
                  Node {val = y, next = nextNode } ->
                       if (x == y) 
                       then do -- if parent deleted simply move ahead
                            b <- atomCAS curPtr curNode (DelNode {next = nextNode})
                            if b 
                            then return True
                            else go prevPtr       -- spin
                       else go curPtr -- continue
                  Null -> return False
                  DelNode {next = nextNode } ->
                         case prevNode of
                              Node {} -> do 
                                   b <- atomCAS prevPtr prevNode (Node {val = val prevNode, next = nextNode})
                                   if b 
                                   then go prevPtr
                                   else go curPtr
                              Head {} -> do 
                                   b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                   if b 
                                   then go prevPtr
                                   else go curPtr
                              DelNode {} -> go curPtr    -- if parent deleted simply move ahead