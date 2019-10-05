{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns, OverloadedStrings, DeriveGeneric #-}
module Models.User
  ( Login (..)
  , ChangePassword (..)
  , User (..)
  , UserName
  , createIO
  , validate
  , validatePassword
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BC
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Models.User.Internal

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

