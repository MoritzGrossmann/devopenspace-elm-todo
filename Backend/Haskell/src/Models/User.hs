{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}
module Models.User
  ( Login (..)
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

type UserName = Text

data Login = Login 
  { name     :: UserName
  , password :: Text
  } deriving Show

$(deriveJSON defaultOptions ''Login)

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