{-# LANGUAGE MagicHash, UnboxedTuples, CPP#-}

module STMStats(getStats, mergeStats)
where

import Data.Map(Map, unionWith, fromList)
import GHC.Base

#ifdef FULL_ABORT
getStats :: IO (Map String [Int])
getStats = 
         IO $ \s# -> case getStats# s# of
                         (# s'#, s1#, s2#, s3#, s4#, s5# #) ->
                            (# s'#, fromList [("Commit Time Full Aborts", [I# s4#]),
                                              ("Successfull Commits", [I# s5#])
                                             ]
                            #)
#else
getStats :: IO (Map String [Int])
getStats = 
         IO $ \s# -> case getStats# s# of
                         (# s'#, s1#, s2#, s3#, s4#, s5# #) ->
                            (# s'#, fromList [("Eager Partial Aborts", [I# s1#]), 
                                              ("Eager Full Aborts", [I# s2#]),
                                              ("Commit Time Partial Aborts", [I# s3#]),
                                              ("Commit Time Full Aborts", [I# s4#]),
                                              ("Successfull Commits", [I# s5#])
                                             ]
                            #)
#endif 

mergeStats :: Map String [Int] -> Map String [Int] -> Map String [Int]
mergeStats s1 s2 = unionWith (++) s1 s2

