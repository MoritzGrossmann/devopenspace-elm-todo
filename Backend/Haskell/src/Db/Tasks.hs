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
import           Models


createTables :: (MonadReader Handle m, MonadIO m) => m ()
createTables = useHandle $ \conn ->
  Sql.execute_ conn
    "CREATE TABLE IF NOT EXISTS todos (\
    \task NVARCHAR(255) NOT NULL, \
    \finished BOOLEAN NOT NULL )"

getTask :: (MonadReader Handle m, MonadIO m) => TaskId -> m (Maybe Task)
getTask tId = useHandle $ \conn ->
  listToMaybe . map toTask <$> Sql.query conn "SELECT task,finished FROM todos WHERE rowid = ?" (Sql.Only tId)
  where
    toTask (txt,fin) = Task tId txt fin


listTasks :: (MonadReader Handle m, MonadIO m) => m [Task]
listTasks = useHandle $ \conn ->
  map toTask <$> Sql.query_ conn "SELECT rowid,task,finished FROM todos"
  where
    toTask (tId,txt,fin) = Task tId txt fin


insertTask :: (MonadReader Handle m, MonadIO m) => Text -> m Task
insertTask txt = useHandle $ \conn -> do
  Sql.execute conn "INSERT INTO todos (task,finished) VALUES (?,0)" (Sql.Only txt)
  tId <- Sql.lastInsertRowId conn
  return $ Task tId txt False


deleteTask :: (MonadReader Handle m, MonadIO m) => TaskId -> m ()
deleteTask tId = useHandle $ \conn ->
  Sql.execute conn "DELETE FROM todos WHERE rowid=?" (Sql.Only tId)


modifyTask :: (MonadReader Handle m, MonadIO m) => Task -> m ()
modifyTask (Task tId txt fin) = useHandle $ \conn ->
  Sql.executeNamed conn "UPDATE todos SET task = :task, finished = :finished WHERE rowid = :id" [":id" := tId, ":task" := txt, ":finished" := fin]