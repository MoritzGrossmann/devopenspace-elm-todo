{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}


module Api.TodosApi
    ( TodosApi
    , serverT
    ) where


import           Authentication (AuthenticatedUser(..))
import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Data.Swagger.Schema (ToSchema)
import           Data.Text (Text)
import           Db (DbHandler, liftHandler)
import qualified Db.Tasks as Db
import           GHC.Generics
import           Imports
import           Models.ListId
import           Models.Tasks (Task, TaskId)
import qualified Models.Tasks as Task
import           Models.User (UserName)
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth)
import qualified Servant.Auth.Server as SAS

newtype TaskText
  = TaskText Text
  deriving (Generic, ToJSON, FromJSON, ToSchema)


type TodosApi =
  Auth '[SA.JWT] AuthenticatedUser :> (
    "list" :> Capture "listId" ListId :>
      "todos" :> (
        Get '[JSON] [Db.Task]
        :<|> ReqBody '[JSON] TaskText :> Post '[JSON] Db.Task
      )
    :<|> "todos" :> (
        ReqBody '[JSON] Db.Task :> Put '[JSON] Db.Task
        :<|> Capture "id" Db.TaskId :> Delete '[JSON] NoContent
        :<|> Capture "id" Db.TaskId :> Get '[JSON] Db.Task
    )
  )

serverT :: ServerT TodosApi DbHandler
serverT = todoHandlers
  where
    todoHandlers (SAS.Authenticated user) =
      withListId userName :<|> withoutListId userName
      where userName = auName user
    todoHandlers _ =
      (\_ -> throwErr401 :<|> (\_ -> throwErr401)) :<|> ((\_ -> throwErr401) :<|> (\_ -> throwErr401) :<|> (\_ -> throwErr401))
      where throwErr401 = liftHandler $ throwError err401
    withListId userName listId =
      getAllHandler userName listId
      :<|> newHandler userName listId
    withoutListId userName =
      updateHandler userName
      :<|> deleteHandler userName
      :<|> queryHandler userName

    checkListAccess :: ListId -> UserName -> DbHandler ()
    checkListAccess lId userName = do
      hasAccess <- Task.hasListAccess userName lId
      if not hasAccess then liftHandler (throwError $ listNotFound lId) else pure ()

    getAllHandler :: UserName -> ListId -> DbHandler [Task]
    getAllHandler userName listId = do
      checkListAccess listId userName
      Task.list userName listId

    updateHandler :: UserName -> Task -> DbHandler Task
    updateHandler userName task = do
      liftIO $ putStrLn $ "updating task " ++ show (Db.id task)
      res <- Task.update userName (Task.id task) task
      if not res
        then liftHandler (throwError err500)
        else do
          found <- Task.get userName (Task.id task)
          case found of
            Nothing -> liftHandler (throwError notFound)
            Just t-> return t

    newHandler :: UserName -> ListId -> TaskText -> DbHandler Task
    newHandler userName listId (TaskText txt) = do
      task <- Task.create userName listId txt
      liftIO $ putStrLn $ "created new task - redirecting to " ++ show (Db.id task)
      return task

    deleteHandler :: UserName -> TaskId -> DbHandler NoContent
    deleteHandler userName tId = do
      deleted <- Task.delete userName tId
      if deleted
        then do
          liftIO $ putStrLn $ "deleted task " ++ show tId
          pure NoContent
        else do
          liftIO $ putStrLn $ "could not delete task " ++ show tId
          liftHandler $ throwError err500

    queryHandler :: UserName -> TaskId -> DbHandler Task
    queryHandler userName tId = do
      liftIO $ putStrLn $ "getting task " ++ show tId
      found <- Task.get userName tId
      case found of
        Nothing -> liftHandler $ throwError notFound
        Just task -> return task

notFound :: ServerError
notFound = err404 { errBody = "sorry - don't know this task" }

listNotFound :: ListId -> ServerError
listNotFound (ListId lId) = err404 { errBody = BS.concat ["List [", BSC.pack (show lId), "] not found"] }
