
import Control.Partial.STM
import Dump


main = do
     x <- atomically $ ((return 1 `orElse` return 2) >>= \t -> if t == 1 then retry else return t) `orElse` return 3
     putStrLn("Result = " ++ show x ++ ", should be 3")
     return()



