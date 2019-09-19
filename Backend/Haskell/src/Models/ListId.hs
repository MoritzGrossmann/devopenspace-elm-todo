{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Models.ListId
  ( ListId (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)
import Servant (FromHttpApiData)


newtype ListId 
  = ListId { getIdValue :: Int64 }
  deriving (Show, Eq, FromJSON, ToJSON, ToField, FromField, FromHttpApiData)