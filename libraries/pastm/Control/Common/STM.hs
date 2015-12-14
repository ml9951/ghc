{-# LANGUAGE CPP, MagicHash, UnboxedTuples,  ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes#-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Full.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PASTM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.  
--
--  * This contains unsafe versions of STM operations
--
-----------------------------------------------------------------------------

module Control.Common.STM
(
    newTVarIO,   
    readTVarIO, 
    writeTVarIO,
    TVar(..),   
    printSTMProfile,
    newTVar#
)
where

import GHC.Base(State#, RealWorld, IO(..), ap, TVar#)
import GHC.Prim(Any, unsafeCoerce#)
import GHC.Conc.Sync(TVar(..))
import Foreign.C.String

readTVarIO :: TVar a -> IO a
readTVarIO (TVar tv) = IO $ \s -> unsafeCoerce# readTVarIO# tv s 

newTVarIO :: a -> IO (TVar a)
newTVarIO x = IO $ \s -> case unsafeCoerce# newTVar# x s of
                              (# s', tv #) -> (# s', TVar tv #)

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO (TVar tv) a = IO $ \s -> case unsafeCoerce# writeTVarIO# tv a s of 
                                          (# s', _ #) -> (# s', () #)

#ifdef STMPROF
printSTMProfile = IO $ \s -> dumpSTMProfile s
#else
printSTMProfile :: IO()
printSTMProfile = return()
#endif

foreign import prim safe "stg_dumpSTMProfile" dumpSTMProfile
        :: State# RealWorld -> (# State# RealWorld, ()#)

#if defined(FTL2) || defined(PTL2) || defined(CHUNKED_TL2) || defined (CHUNKEDTL2)
foreign import prim safe "stg_tl2_newTVarIOzh" newTVar#
        :: Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #) 

foreign import prim safe "stg_tl2_readTVarIOzh" readTVarIO#
        :: Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import prim safe "stg_tl2_writeTVarIOzh" writeTVarIO#
        :: TVar# RealWorld a -> Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #)

#else
foreign import prim safe "stg_norec_newTVarIOzh" newTVar#
        :: Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #) 

foreign import prim safe "stg_norec_readTVarIOzh" readTVarIO#
        :: Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import prim safe "stg_norec_writeTVarIOzh" writeTVarIO#
        :: TVar# RealWorld a -> Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #)
#endif