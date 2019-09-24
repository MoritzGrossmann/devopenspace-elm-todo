{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, DataKinds #-}
module Models.ListId
  ( ListId (..)
  ) where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Swagger.ParamSchema (ToParamSchema)
import Data.Swagger.Schema (ToSchema)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)
import Servant (FromHttpApiData)
import Servant.Docs (ToSample(..), singleSample)


newtype ListId
  = ListId { getIdValue :: Int64 }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToField, FromField, FromHttpApiData, ToParamSchema)

instance ToSample ListId where
  toSamples _ = singleSample $ ListId 42

instance ToSchema ListId
