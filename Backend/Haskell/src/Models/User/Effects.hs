{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns, OverloadedStrings, DeriveGeneric #-}
module Models.User.Effects
  ( UserAction (..)
  , ChangePasswordResult (..)
  , register
  , changePassword
  , get
  , delete
  ) where

import           Control.Effect (Effect)
import           Control.Effect.Carrier (HFunctor, send)
import           GHC.Generics
import           Imports
import           Models.User.Internal

data UserAction m k
  = Get UserName (Maybe User -> m k)
  | Register Login (Maybe User -> m k)
  | UpdatePassword UserName ChangePassword (ChangePasswordResult -> m k)
  | Delete UserName (m k)
  deriving (Functor, Generic1)

instance HFunctor UserAction
instance Effect UserAction

get :: Has UserAction sig m => UserName -> m (Maybe User)
get userName = send (Get userName pure)

register :: Has UserAction sig m => Login -> m (Maybe User)
register login = send (Register login pure)

changePassword :: Has UserAction sig m => UserName -> ChangePassword -> m ChangePasswordResult
changePassword userName pwChange = send (UpdatePassword userName pwChange pure)

delete :: Has UserAction sig m => UserName -> m ()
delete userName = send (Delete userName (pure ()))
