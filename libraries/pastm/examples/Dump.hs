module Dump(trace, traceM) 
where

import GHC.IO

trace :: String -> a -> a
trace msg x =
      unsafePerformIO $ do
                      putStrLn msg
                      return(x)

traceM :: Monad m => String -> m()
traceM msg = trace msg $ return()
