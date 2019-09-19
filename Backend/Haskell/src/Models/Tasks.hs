{-# LANGUAGE TemplateHaskell #-}
module Models.Tasks
  ( TaskId
  , Task (..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Int (Int64)
import Data.Text (Text)
import Models.ListId


type TaskId = Int64


data Task = Task { id       :: TaskId
                 , listId   :: ListId
                 , text     :: Text
                 , finished :: Bool
                 } deriving Show

$(deriveJSON defaultOptions ''Task)
