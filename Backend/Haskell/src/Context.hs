{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Context
  ( Context (..)
  , ContextHandler
  , ContextActionCarrier (..)
  , handleWithinContext
  , getJwtSettings
  , getMaxUsers
  , getMaxListsPerUser
  , getMaxTasksPerList
  ) where

import           Context.Carrier
import           Context.Internal
import           Control.Effect.Reader (Reader, asks)
import           Imports
import qualified Servant.Auth.Server as SAS

getJwtSettings :: Has (Reader Context) sig m => m SAS.JWTSettings
getJwtSettings = asks contextJwtSettings

getMaxUsers :: Has (Reader Context) sig m => m (Maybe Int)
getMaxUsers = asks contextMaxUsers

getMaxListsPerUser :: Has (Reader Context) sig m => m (Maybe Int)
getMaxListsPerUser = asks contextMaxListsPerUser

getMaxTasksPerList :: Has (Reader Context) sig m => m (Maybe Int)
getMaxTasksPerList = asks contextMaxTasksPerList
