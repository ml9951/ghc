
import Control.Partial.STM
import Dump


main = do
     x <- atomically $ (((traceM "returning 1" >>= \_ -> return 1) `orElse` (traceM "returning 2" >>= \_ -> return 2)) >>= \t -> if t == 1 then (traceM ("retrying, t = " ++ show t) >>= \_ -> retry) else return t) `orElse` return 3
     putStrLn("Result = " ++ show x ++ ", should be 3")
     return()



