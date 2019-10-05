{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric #-}
module Models.Tasks
  ( TaskId
  , Task (..)
  , TaskAction (..)
  , hasListAccess
  , get
  , list
  , create
  , update
  , delete
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Int (Int64)
import Data.Swagger.Schema (ToSchema)
import Data.Text (Text)
import GHC.Generics
import Imports
import Models.ListId
import Models.User (UserName)

type TaskId = Int64


data Task = Task { id       :: TaskId
                 , listId   :: ListId
                 , text     :: Text
                 , finished :: Bool
                 } deriving (Generic, Show)

$(deriveJSON defaultOptions ''Task)

instance ToSchema Task


data TaskAction m k
  = CheckAccess UserName ListId (Bool -> m k)
  | Get UserName TaskId (Maybe Task -> m k)
  | List UserName ListId ([Task] -> m k)
  | Create UserName ListId Text (Task -> m k)
  | Update UserName TaskId Task (Bool -> m k)
  | Delete UserName TaskId (Bool -> m k)
  deriving ( Functor, Generic1 )

instance HFunctor TaskAction
instance Effect TaskAction

hasListAccess :: Has TaskAction sig m => UserName -> ListId -> m Bool
hasListAccess userName lId = send (CheckAccess userName lId pure)

get :: Has TaskAction sig m => UserName -> TaskId -> m (Maybe Task)
get userName taskId = send (Get userName taskId pure)

list :: Has TaskAction sig m => UserName -> ListId -> m [Task]
list userName lId = send (List userName lId pure)

create :: Has TaskAction sig m => UserName -> ListId -> Text -> m Task
create userName lId taskText = send (Create userName lId taskText pure)

update :: Has TaskAction sig m => UserName -> TaskId -> Task -> m Bool
update userName taskId updated = send (Update userName taskId updated pure)

delete :: Has TaskAction sig m => UserName -> TaskId -> m Bool
delete userName taskId = send (Delete userName taskId pure)

