{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Db.Lists
  ( ListId
  , List (..)
  , createTables
  , getList
  , listLists
  , insertList
  , deleteList
  , modifyList
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as Sql
import           Db.Internal
import           Models.Lists
import           Models.User (UserName)


createTables :: (MonadReader Handle m, MonadIO m) => m ()
createTables = useHandle $ \conn ->
  Sql.execute_ conn
    "CREATE TABLE IF NOT EXISTS lists (\
    \user NVARCHAR(255) NOT NULL, \
    \name NVARCHAR(255) NOT NULL)"

getList :: (MonadReader Handle m, MonadIO m) => UserName -> ListId -> m (Maybe List)
getList userName lId = useHandle $ \conn ->
  listToMaybe . map toList <$> Sql.queryNamed conn 
    "SELECT lists.name, lists.user, COUNT(*) as cnt \
    \FROM lists, todos \
    \WHERE lists.rowid = :id AND lists.user = :user AND todos.listId = :id \
    \GROUP BY lists.name, lists.user" 
    [ ":id" := lId, ":user" := userName ]
  where
    toList :: (Text, Text, Int) -> List
    toList (ln, _, nr) = List lId ln nr


listLists :: (MonadReader Handle m, MonadIO m) => UserName -> m [List]
listLists userName = useHandle $ \conn ->
  -- TODO: Anzahl offene aus Todos
  map toList <$> Sql.query conn "SELECT rowid,name FROM lists WHERE user=?" (Sql.Only userName)
  where
    toList (lId,ln) = List lId ln 0


insertList :: (MonadReader Handle m, MonadIO m) => UserName -> Text -> m List
insertList userName ln = useHandle $ \conn -> do
  Sql.execute conn "INSERT INTO lists (user,name) VALUES (?,?)" (userName, ln)
  lId <- Sql.lastInsertRowId conn
  return $ List (ListId lId) ln 0


deleteList :: (MonadReader Handle m, MonadIO m) => UserName -> ListId -> m ()
deleteList userName lId = useHandle $ \conn ->
  Sql.execute conn "DELETE FROM lists WHERE rowid=? AND user=?" (lId, userName)


modifyList :: (MonadReader Handle m, MonadIO m) => UserName -> List -> m ()
modifyList userName (List lId ln _) = useHandle $ \conn ->
  Sql.executeNamed conn 
    "UPDATE lists SET name = :name WHERE rowid = :id AND user = :user" 
    [":id" := lId, ":user" := userName, ":name" := ln]