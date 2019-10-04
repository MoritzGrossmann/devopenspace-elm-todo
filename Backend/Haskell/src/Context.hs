{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Context
  ( Context (..)
  , ContextHandler
  , ContextActionCarrier (..)
  , handleWithinContext
  ) where

import           Context.Carrier
import           Context.Internal

