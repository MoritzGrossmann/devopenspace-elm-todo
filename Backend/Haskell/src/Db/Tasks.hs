{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Db.Tasks
  ( TaskId
  , Task (..)
  , createTables
  , getTask
  , listTasks
  , insertTask
  , deleteTask
  , modifyTask
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as Sql
import           Db.Internal
import           Models.ListId
import           Models.Tasks
import           Models.User (UserName)


createTables :: (MonadReader Handle m, MonadIO m) => m ()
createTables = useHandle $ \conn ->
  Sql.execute_ conn
    "CREATE TABLE IF NOT EXISTS todos (\
    \user NVARCHAR(255) NOT NULL, \
    \listId INTEGER NOT NULL, \
    \task NVARCHAR(255) NOT NULL, \
    \finished BOOLEAN NOT NULL )"

getTask :: (MonadReader Handle m, MonadIO m) => UserName -> TaskId -> m (Maybe Task)
getTask userName tId = useHandle $ \conn ->
  listToMaybe . map toTask <$> Sql.queryNamed conn 
    "SELECT listId,task,finished FROM todos WHERE rowid = :id AND user = :user" 
    [ ":id" := tId, ":name" := userName ]
  where
    toTask (lId,txt,fin) = Task tId (ListId lId) txt fin


listTasks :: (MonadReader Handle m, MonadIO m) => UserName -> ListId -> m [Task]
listTasks userName lId = useHandle $ \conn ->
  map toTask <$> Sql.query conn "SELECT rowid,task,finished FROM todos WHERE user=? AND listId=?" (userName, lId)
  where
    toTask (tId,txt,fin) = Task tId lId txt fin


insertTask :: (MonadReader Handle m, MonadIO m) => UserName -> ListId -> Text -> m Task
insertTask userName lId txt = useHandle $ \conn -> do
  Sql.execute conn "INSERT INTO todos (user,listId,task,finished) VALUES (?,?,?,0)" (userName, lId, txt)
  tId <- Sql.lastInsertRowId conn
  return $ Task tId lId txt False


deleteTask :: (MonadReader Handle m, MonadIO m) => UserName -> TaskId -> m ()
deleteTask userName tId = useHandle $ \conn ->
  Sql.execute conn "DELETE FROM todos WHERE rowid=? AND user=?" (tId, userName)


modifyTask :: (MonadReader Handle m, MonadIO m) => UserName -> Task -> m ()
modifyTask userName (Task tId _ txt fin) = useHandle $ \conn ->
  Sql.executeNamed conn 
    "UPDATE todos SET task = :task, finished = :finished WHERE rowid = :id AND user = :user" 
    [":id" := tId, ":user" := userName, ":task" := txt, ":finished" := fin]