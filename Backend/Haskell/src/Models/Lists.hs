{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Lists
  ( ListId (..)
  , List (..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import Models.ListId
import Servant.Docs (ToSample(..), singleSample)


data List = List { id       :: ListId
                 , name     :: Text
                 , nrActive :: Int
                 } deriving Show

$(deriveJSON defaultOptions ''List)

instance ToSample List where
  toSamples _ = singleSample $ List (ListId 42) "Listen-Name" 11
