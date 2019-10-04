{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Models.Lists
  ( ListId (..)
  , List (..)
  , ListAction (..)
  , exists, getOne, getAll, create, delete, modify
  ) where

import Imports
import GHC.Generics
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import Data.Swagger.Schema (ToSchema)
import Models.ListId
import Models.User (UserName)
import Control.Effect (Effect)
import Control.Effect.Carrier (HFunctor, send)


data List = List { id       :: ListId
                 , name     :: Text
                 , nrActive :: Int
                 } deriving (Generic, Show)

$(deriveJSON defaultOptions ''List)

instance ToSchema List

data ListAction m k
  = Exists UserName ListId (Bool -> m k)
  | GetOne UserName ListId (Maybe List -> m k)
  | GetAll UserName ([List] -> m k)
  | Create UserName Text (List -> m k)
  | Delete UserName ListId (m k)
  | Modify UserName List (m k)
  deriving ( Functor, Generic1 )

instance HFunctor ListAction
instance Effect ListAction

exists :: Has ListAction sig m => UserName -> ListId -> m Bool
exists userName listId = send (Exists userName listId pure) 

getOne :: Has ListAction sig m => UserName -> ListId -> m (Maybe List)
getOne userName listId = send (GetOne userName listId pure) 

getAll :: Has ListAction sig m => UserName -> m [List]
getAll userName = send (GetAll userName pure) 

create :: Has ListAction sig m => UserName -> Text -> m List
create userName listName = send (Create userName listName pure) 

delete :: Has ListAction sig m => UserName -> ListId -> m ()
delete userName listId = send (Delete userName listId (pure ())) 

modify :: Has ListAction sig m => UserName -> List -> m ()
modify userName list = send (Modify userName list (pure ())) 
