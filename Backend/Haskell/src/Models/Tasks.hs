{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric #-}
module Models.Tasks
  ( TaskId
  , Task (..)
  ) where

import GHC.Generics
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Swagger.Schema (ToSchema)
import Models.ListId
import Servant.Docs


type TaskId = Int64


data Task = Task { id       :: TaskId
                 , listId   :: ListId
                 , text     :: Text
                 , finished :: Bool
                 } deriving (Generic, Show)

$(deriveJSON defaultOptions ''Task)

instance ToSample Task where
  toSamples _ = singleSample $ Task 4711 (ListId 42) "Task-Name" False

instance ToSchema Task
