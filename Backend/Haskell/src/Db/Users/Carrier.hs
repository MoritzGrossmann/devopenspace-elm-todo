{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Db.Users.Carrier
  ( UsersTag
  , runUserActionDb
  ) where

import qualified Context
import           Context.Internal
import           Control.Effect.Reader (Reader)
import           Control.Monad (when)
import           Db.Carrier (ActionDbCarrier(..), liftDb)
import           Db.Users as Db
import           Imports
import           Models.User
import           Models.User.Effects

data UsersTag

runUserActionDb :: ActionDbCarrier UsersTag m a -> m a
runUserActionDb = runActionDbCarrier

instance (Carrier sig m, MonadIO m, Has (Reader Context) sig m)
  => Carrier(UserAction :+: sig) (ActionDbCarrier UsersTag m) where
  eff (R other) = ActionDbCarrier (eff $ handleCoercible other)
  eff (L (Register login k)) = do
    maxUsers <- Context.getMaxUsers
    registered <- liftDb $ do
      userRes <- createIO login
      userCount <- Db.countUsers
      when (maybe False (userCount >=) maxUsers) (error "too many users registered")
      case userRes of
        Nothing -> pure Nothing
        Just user -> do
          insertUser user
          pure $ Just user
    k registered
  eff (L (UpdatePassword userName ChangePassword{..} k)) = k =<< (liftDb $ do
      foundUser <- getUser userName
      case foundUser of
        Nothing -> pure ChPwdInvalidUser
        Just user | not (validatePassword user oldPassword) -> pure ChPwdInvalidPassword
        Just _ -> do
          createRes <- createIO (Login userName newPassword)
          case createRes of
            Just changedUser -> do
              updateUser userName changedUser
              pure ChPwdSuccess
            Nothing -> pure ChPwdInternalError)
  eff (L (Get userName k)) =
    liftDb (getUser userName) >>= k
  eff (L (Delete userName k)) =
    liftDb (deleteUser userName) >> k
