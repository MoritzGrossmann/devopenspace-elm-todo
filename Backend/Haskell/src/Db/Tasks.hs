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
import           Models.Tasks
import           Models.User (UserName)


createTables :: (MonadReader Handle m, MonadIO m) => m ()
createTables = useHandle $ \conn ->
  Sql.execute_ conn
    "CREATE TABLE IF NOT EXISTS todos (\
    \user NVARCHAR(255) NOT NULL, \
    \task NVARCHAR(255) NOT NULL, \
    \finished BOOLEAN NOT NULL )"

getTask :: (MonadReader Handle m, MonadIO m) => UserName -> TaskId -> m (Maybe Task)
getTask userName tId = useHandle $ \conn ->
  listToMaybe . map toTask <$> Sql.queryNamed conn 
    "SELECT task,finished FROM todos WHERE rowid = :id AND user = :user" 
    [ ":id" := tId, ":name" := userName ]
  where
    toTask (txt,fin) = Task tId txt fin


listTasks :: (MonadReader Handle m, MonadIO m) => UserName -> m [Task]
listTasks userName = useHandle $ \conn ->
  map toTask <$> Sql.query conn "SELECT rowid,task,finished FROM todos WHERE user=?" (Sql.Only userName)
  where
    toTask (tId,txt,fin) = Task tId txt fin


insertTask :: (MonadReader Handle m, MonadIO m) => UserName -> Text -> m Task
insertTask userName txt = useHandle $ \conn -> do
  Sql.execute conn "INSERT INTO todos (user,task,finished) VALUES (?,?,0)" (userName, txt)
  tId <- Sql.lastInsertRowId conn
  return $ Task tId txt False


deleteTask :: (MonadReader Handle m, MonadIO m) => UserName -> TaskId -> m ()
deleteTask userName tId = useHandle $ \conn ->
  Sql.execute conn "DELETE FROM todos WHERE rowid=? AND user=?" (tId, userName)


modifyTask :: (MonadReader Handle m, MonadIO m) => UserName -> Task -> m ()
modifyTask userName (Task tId txt fin) = useHandle $ \conn ->
  Sql.executeNamed conn 
    "UPDATE todos SET task = :task, finished = :finished WHERE rowid = :id AND user = :user" 
    [":id" := tId, ":user" := userName, ":task" := txt, ":finished" := fin]