{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns, OverloadedStrings, DeriveGeneric #-}
module Models.User
  ( Login (..)
  , ChangePassword (..)
  , User (..)
  , UserName
  , UserAction (..)
  , get
  , create
  , update
  , delete
  , createIO
  , validate
  , validatePassword
  ) where

import           Control.Effect (Effect)
import           Control.Effect.Carrier (HFunctor, send)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BC
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.ByteString (ByteString)
import           Data.Swagger.ParamSchema (ToParamSchema(..))
import           Data.Swagger.Schema (ToSchema)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics
import           Imports

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

data User = User
  { userName   :: UserName
  , userPwHash :: ByteString
  } deriving Show

data UserAction m k
  = Get UserName (Maybe User -> m k)
  | Insert User (m k)
  | Update UserName User (m k)
  | Delete UserName (m k)
  deriving (Functor, Generic1)

instance HFunctor UserAction
instance Effect UserAction

get :: Has UserAction sig m => UserName -> m (Maybe User)
get userName = send (Get userName pure)

create :: (MonadIO m, Has UserAction sig m) => Login -> m (Maybe User)
create login = do
  user' <- liftIO $ createIO login
  case user' of
    Just user -> do
      send (Insert user (pure ()))
      pure (Just user)
    Nothing -> pure Nothing

update :: Has UserAction sig m => UserName -> User -> m ()
update userName user = send (Update userName user (pure ()))

delete :: Has UserAction sig m => UserName -> m ()
delete userName = send (Delete userName (pure ()))


createIO :: MonadIO m => Login -> m (Maybe User)
createIO Login{..} =
  fmap (User name) <$> pwHash
  where
  pwHash = do
    let encoded = encodeUtf8 password
    liftIO $ BC.hashPasswordUsingPolicy BC.fastBcryptHashingPolicy encoded

validate :: User -> Login -> Bool
validate User{userPwHash} Login{password} =
  BC.validatePassword userPwHash $ encodeUtf8 password

validatePassword :: User -> Text -> Bool
validatePassword User{userPwHash} pwd =
  BC.validatePassword userPwHash $ encodeUtf8 pwd

