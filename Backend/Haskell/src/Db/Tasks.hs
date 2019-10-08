{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
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
  , TaskAction (..)
  , TasksTag
  , runTasksActionDb
  ) where

import           Context.Internal
import           Control.Effect.Reader (Reader)
import           Control.Monad (when)
import           Control.Monad.Reader (MonadReader)
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Text (Text)
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as Sql
import           Db.Carrier (ActionDbCarrier(..), liftDb)
import           Db.Internal
import qualified Db.Lists as DbL
import           Imports
import           Models.ListId
import           Models.Tasks
import           Models.User (UserName)


data TasksTag

runTasksActionDb :: ActionDbCarrier TasksTag m a -> m a
runTasksActionDb = runActionDbCarrier


maxTasksPerList :: Int
maxTasksPerList = 10


instance (Carrier sig m, MonadIO m, Has (Reader Context) sig m)
  => Carrier (TaskAction :+: sig) (ActionDbCarrier TasksTag m) where
  eff (R other) = ActionDbCarrier (eff $ handleCoercible other)
  eff (L (CheckAccess userName lId k)) = k =<< (liftDb $ DbL.listExists userName lId)
  eff (L (Get userName taskId k)) = k =<< (liftDb $ getTask userName taskId)
  eff (L (List userName lId k)) = k =<< (liftDb $ listTasks userName lId)
  eff (L (Create userName lId taskText k)) = k =<< (liftDb $ do
    taskCount <- countTasks userName lId
    when (taskCount >= maxTasksPerList) (error "too many task for this list")
    insertTask userName lId taskText)
  eff (L (Delete userName taskId k)) = k =<< (liftDb $ do
    deleteTask userName taskId
    pure True )
  eff (L (Update userName taskId task k)) = k =<< (liftDb $ do
    modifyTask userName taskId task
    pure True )

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
    [ ":id" := tId, ":user" := userName ]
  where
    toTask (lId,txt,fin) = Task tId (ListId lId) txt fin


countTasks :: (MonadReader Handle m, MonadIO m) => UserName -> ListId -> m Int
countTasks userName lId = useHandle $ \conn ->
  fromMaybe 0 . listToMaybe . map toCount <$> Sql.queryNamed conn
    "SELECT COUNT(*) as cnt, 1 as one FROM todos where user = :user AND listId = :listId"
    [ ":user" := userName, ":listId" := lId ]
  where
    toCount :: (Int, Int) -> Int
    toCount (cnt, _) = cnt


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


modifyTask :: (MonadReader Handle m, MonadIO m) => UserName -> TaskId -> Task -> m ()
modifyTask userName tId (Task _ _ txt fin) = useHandle $ \conn ->
  Sql.executeNamed conn
    "UPDATE todos SET task = :task, finished = :finished WHERE rowid = :id AND user = :user"
    [":id" := tId, ":user" := userName, ":task" := txt, ":finished" := fin]
