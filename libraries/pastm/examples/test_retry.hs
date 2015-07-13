
import Control.Concurrent.PASTM


alt1 x = do
     t <- readTVar x
     writeTVar x (t + 12)
     retry

alt2 x = do
     t <- readTVar x
     writeTVar x (t + 1)     

retryTest = do
          x <- newTVar 0
          y <- newTVar 0
          alt1 x `orElse` alt2 y
          tx <- readTVar x
          ty <- readTVar y
          return(tx, ty)

main = do
     (x, y) <- atomically $ retryTest 
     putStrLn("x = " ++ show x ++ ", y = " ++ show y)
     return()



