{-# LANGUAGE CPP, MagicHash, UnboxedTuples, ForeignFunctionInterface #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TVar
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- TVar: Transactional variables
--
-----------------------------------------------------------------------------

module Control.STMHaskell.STM (
        -- * TVars
        STM,
        TVar,
        atomically,
        newTVar,
        readTVar,
        writeTVar,
        printStats
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Conc
#else
import Control.Sequential.STM
#endif

foreign import ccall "stmPrintStats" printStats :: IO()




