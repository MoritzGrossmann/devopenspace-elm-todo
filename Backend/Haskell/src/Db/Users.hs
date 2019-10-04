{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Db.Users
  ( User (..)
  , createTables
  , getUser
  , insertUser
  , deleteUser
  , updateUser
  , UsersTag
  , runUserActionDb
  ) where

import           Context.Internal
import           Control.Effect.Carrier (Carrier(..), (:+:)(..), handleCoercible)
import           Control.Effect.Reader (Reader)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Maybe (listToMaybe)
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as Sql
import           Db.Carrier (ActionDbCarrier(..), liftDb)
import           Db.Internal
import           Imports
import           Models.User

data UsersTag

runUserActionDb :: ActionDbCarrier UsersTag m a -> m a
runUserActionDb = runActionDbCarrier

instance (Carrier sig m, MonadIO m, Has (Reader Context) sig m)
  => Carrier(UserAction :+: sig) (ActionDbCarrier UsersTag m) where
  eff (R other) = ActionDbCarrier (eff $ handleCoercible other)
  eff (L (Register login k)) = k =<< (liftDb $ do
    userRes <- createIO login
    case userRes of
      Nothing -> pure Nothing
      Just user -> do
        insertUser user
        pure $ Just user)
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

createTables :: (MonadReader Handle m, MonadIO m) => m ()
createTables = useHandle $ \conn ->
  Sql.execute_ conn
    "CREATE TABLE IF NOT EXISTS users (\
    \name NVARCHAR(255) PRIMARY KEY, \
    \pwHash NVARCHAR(255) NOT NULL) WITHOUT ROWID"

getUser :: (MonadReader Handle m, MonadIO m) => UserName -> m (Maybe User)
getUser userName = useHandle $ \conn ->
  listToMaybe . map toUser <$> Sql.query conn "SELECT name, pwHash FROM users WHERE name = ?" (Sql.Only userName)
  where
    toUser (name, hash) = User name hash


insertUser :: (MonadReader Handle m, MonadIO m) => User -> m ()
insertUser User{..} = useHandle $ \conn ->
  Sql.executeNamed conn "INSERT INTO users (name, pwHash) VALUES (:name,:hash)" [ ":name" := userName, ":hash" := userPwHash ]


deleteUser :: (MonadReader Handle m, MonadIO m) => UserName -> m ()
deleteUser userName = useHandle $ \conn ->
  Sql.execute conn "DELETE FROM users WHERE name=?" (Sql.Only userName)


updateUser :: (MonadReader Handle m, MonadIO m) => UserName -> User -> m ()
updateUser oldName User{..} = useHandle $ \conn ->
  Sql.executeNamed conn "UPDATE users SET name = :name, pwHash = :hash WHERE name = :oldName" [":oldName" := oldName, ":name" := userName, ":hash" := userPwHash]
