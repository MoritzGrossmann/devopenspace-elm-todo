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
    , server
    ) where


import           GHC.Generics
import           Authentication (AuthenticatedUser(..), hoistServerWithAuth)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Data.Swagger.Schema (ToSchema)
import           Data.Text (Text)
import qualified Db
import qualified Db.Lists as DbL
import qualified Db.Tasks as Db
import           Models.ListId
import           Servant
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth)
import qualified Servant.Auth.Server as SAS
import           Servant.Docs

newtype TaskText 
  = TaskText Text
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance ToSample TaskText where
  toSamples _ = singleSample $ 
     TaskText "Text des neuen Tasks"


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

instance ToCapture (Capture "id" Db.TaskId) where
  toCapture _ = DocCapture "id" "ID des Tasks der benutzt werden soll"

instance ToCapture (Capture "listId" ListId) where
  toCapture _ = DocCapture "id" "ID der Liste die benutzt werden soll"


server :: Db.Handle -> Server TodosApi
server dbHandle = hoistServerWithAuth (Proxy :: Proxy TodosApi) toHandle todoHandlers
  where
    todoHandlers (SAS.Authenticated user) = withListId userName :<|> withoutListId userName
      where userName = auName user
    todoHandlers _ = SAS.throwAll err401
    withListId userName listId =
      getAllHandler userName listId
      :<|> newHandler userName listId
    withoutListId userName =
      updateHandler userName
      :<|> deleteHandler userName
      :<|> queryHandler userName

    checkListAccess listId userName = do
      exists <- DbL.listExists userName listId
      if not exists then throwError (listNotFound listId) else pure ()

    getAllHandler userName listId = do
      checkListAccess listId userName
      Db.listTasks userName listId

    updateHandler userName task = do
      liftIO $ putStrLn $ "updating task " ++ show (Db.id task)
      Db.modifyTask userName task
      found <- Db.getTask userName (Db.id task)
      case found of
        Nothing -> throwError notFound
        Just t-> return t

    newHandler userName listId (TaskText txt) = do
      task <- Db.insertTask userName listId txt
      liftIO $ putStrLn $ "created new task - redirecting to " ++ show (Db.id task)
      return task

    deleteHandler userName tId = do
      Db.deleteTask userName tId
      liftIO $ putStrLn $ "deleted task " ++ show tId
      pure NoContent

    queryHandler userName tId = do
      liftIO $ putStrLn $ "getting task " ++ show tId
      found <- Db.getTask userName tId
      case found of
        Nothing -> throwError notFound
        Just task -> return task

    toHandle :: ReaderT Db.Handle Handler a -> Handler a
    toHandle r = runReaderT r dbHandle


notFound :: ServerError
notFound = err404 { errBody = "sorry - don't know this task" }

listNotFound :: ListId -> ServerError
listNotFound (ListId lId) = err404 { errBody = BS.concat ["List [", BSC.pack (show lId), "] not found"] }
