{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns, OverloadedStrings, DeriveGeneric #-}
module Models.User.Internal
  ( Login (..)
  , ChangePassword (..)
  , ChangePasswordResult (..)
  , User (..)
  , UserName
  ) where

import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.ByteString (ByteString)
import           Data.Swagger.ParamSchema (ToParamSchema(..))
import           Data.Swagger.Schema (ToSchema)
import           Data.Text (Text)
import           GHC.Generics

type UserName = Text

data Login = Login
  { name     :: UserName
  , password :: Text
  } deriving (Generic, Show)

$(deriveJSON defaultOptions ''Login)

instance ToSchema Login
instance ToParamSchema Login where
  toParamSchema _ = mempty

data ChangePassword = ChangePassword
  { oldPassword :: Text
  , newPassword :: Text
  } deriving (Generic, Show)

$(deriveJSON defaultOptions ''ChangePassword)

instance ToSchema ChangePassword
instance ToParamSchema ChangePassword where
  toParamSchema _ = mempty


data ChangePasswordResult
  = ChPwdSuccess
  | ChPwdInvalidUser
  | ChPwdInvalidPassword
  | ChPwdInternalError

data User = User
  { userName   :: UserName
  , userPwHash :: ByteString
  } deriving Show
