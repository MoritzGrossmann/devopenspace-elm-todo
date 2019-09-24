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


data List = List { id       :: ListId
                 , name     :: Text
                 , nrActive :: Int
                 } deriving (Generic, Show)

$(deriveJSON defaultOptions ''List)

instance ToSchema List
