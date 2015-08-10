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

module Control.IO.STM
(
    newTVarIO,   
    readTVarIO, 
    writeTVarIO,
    TVar(..),    
)
where

import GHC.Base(State#, RealWorld, IO(..), ap, TVar#)
import GHC.Prim(Any, unsafeCoerce#)
import GHC.Conc.Sync(TVar(..))

readTVarIO :: TVar a -> IO a
readTVarIO (TVar tv) = IO $ \s -> unsafeCoerce# readTVarIO# tv s 

newTVarIO :: a -> IO (TVar a)
newTVarIO x = IO $ \s -> case unsafeCoerce# newTL2TVar# x s of
                              (# s', tv #) -> (# s', TVar tv #)

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO (TVar tv) a = IO $ \s -> case unsafeCoerce# writeTVarIO# tv a s of 
                                          (# s', _ #) -> (# s', () #)

#if defined(TL2) || defined(PTL2)
foreign import prim safe "stg_tl2_newTVarIOzh" newTL2TVar#
        :: Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #) 

foreign import prim safe "stg_tl2_readTVarIOzh" readTVarIO#
        :: Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import prim safe "stg_tl2_writeTVarIOzh" writeTVarIO#
        :: TVar# RealWorld a -> Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #)

#else
foreign import prim safe "stg_norec_newTVarIOzh" newTL2TVar#
        :: Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #) 

foreign import prim safe "stg_norec_readTVarIOzh" readTVarIO#
        :: Any() -> State# RealWorld -> (# State# RealWorld, a #)

foreign import prim safe "stg_norec_writeTVarIOzh" writeTVarIO#
        :: TVar# RealWorld a -> Any() -> State# RealWorld -> (# State# RealWorld, TVar# RealWorld a #)
#endif