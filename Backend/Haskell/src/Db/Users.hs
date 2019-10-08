{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Db.Users
  ( User (..)
  , createTables
  , countUsers
  , getUser
  , insertUser
  , deleteUser
  , updateUser
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Maybe (listToMaybe, fromMaybe)
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as Sql
import           Db.Internal
import           Models.User

createTables :: (MonadReader Handle m, MonadIO m) => m ()
createTables = useHandle $ \conn ->
  Sql.execute_ conn
    "CREATE TABLE IF NOT EXISTS users (\
    \name NVARCHAR(255) PRIMARY KEY, \
    \pwHash NVARCHAR(255) NOT NULL) WITHOUT ROWID"


countUsers :: (MonadReader Handle m, MonadIO m) => m Int
countUsers = useHandle $ \conn -> do
  (rows :: [(Int, Int)]) <- Sql.query_ conn "SELECT COUNT(name) as cnt, 1 as one FROM users"
  pure $ fromMaybe 0 $ listToMaybe $ map fst rows


getUser :: (MonadReader Handle m, MonadIO m) => UserName -> m (Maybe User)
getUser userName = useHandle $ \conn ->
  listToMaybe . map toUser <$> Sql.query conn "SELECT name, pwHash FROM users WHERE name = ?" (Sql.Only userName)
  where
    toUser (name, hash) = User name hash


insertUser :: (MonadReader Handle m, MonadIO m) => User -> m ()
insertUser User{..} = useHandle $ \conn ->
  Sql.executeNamed conn "INSERT INTO users (name, pwHash) VALUES (:name,:hash)" [ ":name" := userName, ":hash" := userPwHash ]


deleteUser :: (MonadReader Handle m, MonadIO m) => UserName -> m ()
deleteUser userName = useHandle $ \conn -> do
  Sql.execute conn "DELETE FROM todos WHERE user=?" (Sql.Only userName)
  Sql.execute conn "DELETE FROM lists WHERE user=?" (Sql.Only userName)
  Sql.execute conn "DELETE FROM users WHERE name=?" (Sql.Only userName)


updateUser :: (MonadReader Handle m, MonadIO m) => UserName -> User -> m ()
updateUser oldName User{..} = useHandle $ \conn ->
  Sql.executeNamed conn "UPDATE users SET name = :name, pwHash = :hash WHERE name = :oldName" [":oldName" := oldName, ":name" := userName, ":hash" := userPwHash]
