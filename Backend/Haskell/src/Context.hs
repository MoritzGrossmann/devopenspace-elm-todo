{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Context
  ( Context (..)
  , ContextHandler
  , ContextActionCarrier (..)
  , handleWithinContext
  , getJwtSettings
  ) where

import           Context.Carrier
import           Context.Internal
import           Control.Effect.Reader (Reader, asks)
import           Imports
import qualified Servant.Auth.Server as SAS

getJwtSettings :: Has (Reader Context) sig m => m SAS.JWTSettings
getJwtSettings = asks contextJwtSettings
