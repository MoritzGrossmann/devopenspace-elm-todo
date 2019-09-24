{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns, OverloadedStrings, DeriveGeneric #-}
module Models.User
  ( Login (..)
  , ChangePassword (..)
  , User (..)
  , UserName
  , create
  , validate
  , validatePassword
  ) where

import           GHC.Generics
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BC
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Swagger.Schema (ToSchema)
import           Data.Swagger.ParamSchema (ToParamSchema(..))

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


create :: MonadIO m => Login -> m (Maybe User)
create Login{..} =
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

