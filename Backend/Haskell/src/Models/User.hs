{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns, OverloadedStrings #-}
module Models.User
  ( Login (..)
  , ChangePassword (..)
  , User (..)
  , UserName
  , create
  , validate
  , validatePassword
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BC
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Servant.Docs (ToSample (..), singleSample)

type UserName = Text

data Login = Login
  { name     :: UserName
  , password :: Text
  } deriving Show

$(deriveJSON defaultOptions ''Login)

instance ToSample Login where
  toSamples _ = singleSample $ Login "your-username" "top secret pa$$w0rd"

data ChangePassword = ChangePassword
  { oldPassword :: Text
  , newPassword :: Text
  } deriving Show

$(deriveJSON defaultOptions ''ChangePassword)

instance ToSample ChangePassword where
  toSamples _ = singleSample $ ChangePassword "oldPassword" "newPassword"

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

