{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Lists
  ( ListId (..)
  , List (..)
  ) where

import GHC.Generics
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import Data.Swagger.Schema (ToSchema)
import Models.ListId
import Servant.Docs (ToSample(..), singleSample)


data List = List { id       :: ListId
                 , name     :: Text
                 , nrActive :: Int
                 } deriving (Generic, Show)

$(deriveJSON defaultOptions ''List)

instance ToSample List where
  toSamples _ = singleSample $ List (ListId 42) "Listen-Name" 11

instance ToSchema List
