{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Models.Tasks
  ( TaskId
  , Task (..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Int (Int64)
import Data.Text (Text)
import Models.ListId
import Servant.Docs


type TaskId = Int64


data Task = Task { id       :: TaskId
                 , listId   :: ListId
                 , text     :: Text
                 , finished :: Bool
                 } deriving Show

$(deriveJSON defaultOptions ''Task)

instance ToSample Task where
  toSamples _ = singleSample $ Task 4711 (ListId 42) "Task-Name" False
