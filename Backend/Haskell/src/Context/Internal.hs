{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Context.Internal
  ( Context (..)
  ) where

import qualified Db.Internal as Db
import qualified Servant.Auth.Server as SAS

data Context = Context
  { contextDbHandle :: Db.Handle
  , contextJwtSettings :: SAS.JWTSettings
  , contextMaxUsers :: Maybe Int
  , contextMaxListsPerUser :: Maybe Int
  , contextMaxTasksPerList :: Maybe Int
  }
