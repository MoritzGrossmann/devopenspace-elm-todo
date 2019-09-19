{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Models.Lists
  ( ListId (..)
  , List (..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import Models.ListId


data List = List { id       :: ListId
                 , name     :: Text
                 , nrActive :: Int
                 } deriving Show

$(deriveJSON defaultOptions ''List)
