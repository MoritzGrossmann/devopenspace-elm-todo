{-# LANGUAGE OverloadedStrings, 
             FlexibleContexts, 
             ScopedTypeVariables, 
             GeneralizedNewtypeDeriving,
             TypeFamilies,
             TypeOperators,
             FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances
#-}

module Db.Lists
  ( ListId
  , List (..)
  , createTables
  , listExists
  , countLists
  , getList
  , listLists
  , insertList
  , deleteList
  , modifyList
  , ListsTag
  , runListsActionDb
  ) where

import qualified Context
import           Context.Internal
import           Control.Effect.Carrier (Carrier(..), (:+:)(..), handleCoercible)
import           Control.Effect.Reader (Reader)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Text (Text)
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as Sql
import           Db.Carrier (ActionDbCarrier(..), liftDb)
import           Db.Internal
import           Imports
import           Models.Lists (ListAction(..), ListId(..), List(..))
import           Models.User (UserName)


data ListsTag

runListsActionDb :: ActionDbCarrier ListsTag m a -> m a
runListsActionDb = runActionDbCarrier

instance (Carrier sig m, MonadIO m, Has (Reader Context) sig m)
  => Carrier (ListAction :+: sig) (ActionDbCarrier ListsTag m) where
  eff (R other) = ActionDbCarrier (eff $ handleCoercible other)
  eff (L (Exists userName listId k)) =
    liftDb (listExists userName listId) >>= k
  eff (L (GetOne userName listId k)) =
    liftDb (getList userName listId) >>= k
  eff (L (GetAll userName k)) =
    liftDb (listLists userName) >>= k
  eff (L (Create userName text k)) = do
    maxListsPerUser <- Context.getMaxListsPerUser
    inserted <- liftDb $ do
      listCount <- countLists userName
      when (maybe False (listCount >=) maxListsPerUser) (error "too many lists for this user")
      insertList userName text
    k inserted
  eff (L (Delete userName listId k)) = do
    liftDb $ deleteList userName listId
    k
  eff (L (Modify userName list k)) =
    liftDb (modifyList userName list) >> k

createTables :: (MonadReader Handle m, MonadIO m) => m ()
createTables = useHandle $ \conn ->
  Sql.execute_ conn
    "CREATE TABLE IF NOT EXISTS lists (\
    \user NVARCHAR(255) NOT NULL, \
    \name NVARCHAR(255) NOT NULL)"


listExists :: (MonadReader Handle m, MonadIO m) => UserName -> ListId -> m Bool
listExists userName lId = useHandle $ \conn -> do
  (rows :: [(Int, Int)]) <- Sql.queryNamed conn
    "SELECT rowid, 1 FROM lists WHERE rowid = :id AND user = :name LIMIT 1"
    [ ":id" := lId, ":name" := userName ]
  pure $ not $ null rows


countLists :: (MonadReader Handle m, MonadIO m) => UserName -> m Int
countLists userName = useHandle $ \conn ->
  fromMaybe 0 . listToMaybe . map toCount <$> Sql.queryNamed conn
    "SELECT COUNT(*) as cnt, 1 as one FROM lists where user = :user"
    [ ":user" := userName ]
  where
    toCount :: (Int, Int) -> Int
    toCount (cnt, _) = cnt


getList :: (MonadReader Handle m, MonadIO m) => UserName -> ListId -> m (Maybe List)
getList userName lId = useHandle $ \conn ->
  listToMaybe . map toList <$> Sql.queryNamed conn
    "SELECT lists.name, lists.user, (SELECT COUNT(*) FROM todos WHERE todos.listId = :id AND todos.finished = 0) as cnt \
    \FROM lists \
    \WHERE lists.rowid = :id AND lists.user = :user"
    [ ":id" := lId, ":user" := userName ]
  where
    toList :: (Text, Text, Int) -> List
    toList (ln, _, cnt) = List lId ln cnt


listLists :: (MonadReader Handle m, MonadIO m) => UserName -> m [List]
listLists userName = useHandle $ \conn ->
  map toList <$> Sql.queryNamed conn
    "SELECT lists.rowid, lists.name, (SELECT COUNT(*) FROM todos WHERE todos.listId = lists.rowid AND todos.finished = 0) as cnt \
    \FROM lists \
    \WHERE lists.user=:user"
    [ ":user" := userName ]
  where
    toList (lId,ln,cnt) = List lId ln cnt


insertList :: (MonadReader Handle m, MonadIO m) => UserName -> Text -> m List
insertList userName ln = useHandle $ \conn -> do
  Sql.execute conn "INSERT INTO lists (user,name) VALUES (?,?)" (userName, ln)
  lId <- Sql.lastInsertRowId conn
  return $ List (ListId lId) ln 0


deleteList :: (MonadReader Handle m, MonadIO m) => UserName -> ListId -> m ()
deleteList userName lId = useHandle $ \conn -> do
  Sql.execute conn "DELETE FROM todos WHERE listId=? AND user=?" (lId, userName)
  Sql.execute conn "DELETE FROM lists WHERE rowid=? AND user=?" (lId, userName)


modifyList:: (MonadReader Handle m, MonadIO m) => UserName -> List -> m ()
modifyList userName (List lId ln _) = useHandle $ \conn ->
  Sql.executeNamed conn
    "UPDATE lists SET name = :name WHERE rowid = :id AND user = :user"
    [":id" := lId, ":user" := userName, ":name" := ln]
