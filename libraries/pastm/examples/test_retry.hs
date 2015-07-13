
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
          alt1 x `orElse` alt2 x
          t <- readTVar x
          return(t)

main = do
     x <- atomically $ retryTest 
     putStrLn("Result = " ++ show x)
     return()



