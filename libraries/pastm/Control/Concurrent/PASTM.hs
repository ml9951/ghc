{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.PASTM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PASTM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.  See
--
--  * /Composable memory transactions/, by Tim Harris, Simon Marlow, Simon
--    Peyton Jones, and Maurice Herlihy, in /ACM Conference on Principles
--    and Practice of Parallel Programming/ 2005.
--    <http://research.microsoft.com/Users/simonpj/papers/stm/index.htm>
--
-----------------------------------------------------------------------------

module Control.Concurrent.PASTM (
--        module Control.Monad.PASTM,
        module Control.Concurrent.PASTM.TVar,
#ifdef __GLASGOW_HASKELL__
        module Control.Concurrent.PASTM.TMVar,
        module Control.Concurrent.PASTM.TChan,
        module Control.Concurrent.PASTM.TQueue,
        module Control.Concurrent.PASTM.TBQueue,
        module Control.Concurrent.PASTM.Core,   
#endif
        module Control.Concurrent.PASTM.TArray
  ) where

--import Control.Monad.PASTM
import Control.Concurrent.PASTM.TVar
#ifdef __GLASGOW_HASKELL__
import Control.Concurrent.PASTM.TMVar
import Control.Concurrent.PASTM.TChan
import Control.Concurrent.PASTM.Core
#endif
import Control.Concurrent.PASTM.TArray
import Control.Concurrent.PASTM.TQueue
import Control.Concurrent.PASTM.TBQueue
