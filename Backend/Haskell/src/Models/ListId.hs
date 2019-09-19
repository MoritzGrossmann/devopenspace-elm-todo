{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Models.ListId
  ( ListId (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)
import Servant (FromHttpApiData)
import Servant.Docs (ToSample(..), singleSample)


newtype ListId
  = ListId { getIdValue :: Int64 }
  deriving (Show, Eq, FromJSON, ToJSON, ToField, FromField, FromHttpApiData)

instance ToSample ListId where
  toSamples _ = singleSample $ ListId 42
