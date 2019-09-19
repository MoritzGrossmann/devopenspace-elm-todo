{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module Api.TodosApi
    ( TodosApi
    , server
    ) where


import           Api.UsersApi (AuthenticatedUser)
import qualified Api.UsersApi as UsersApi
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import           Data.Text (Text)
import qualified Db
import qualified Db.Tasks as Db
import qualified Db.Lists as DbL
import           Models.ListId
import           Servant
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth)
import qualified Servant.Auth.Server as SAS


type TodosApi =
  Auth '[SA.JWT] AuthenticatedUser :>
  "list" :> Capture "listId" ListId :>
  "todos" :> (
    Get '[JSON] [Db.Task]
    :<|> ReqBody '[JSON] Db.Task :> Put '[JSON] Db.Task
    :<|> ReqBody '[JSON] Text :> Post '[JSON] Db.Task
    :<|> Capture "id" Db.TaskId :> Delete '[JSON] [Db.Task]
    :<|> Capture "id" Db.TaskId :> Get '[JSON] Db.Task
  )


server :: Db.Handle -> Server TodosApi
server dbHandle = UsersApi.hoistServerWithAuth (Proxy :: Proxy TodosApi) toHandle todoHandlers
  where
    todoHandlers (SAS.Authenticated user) listId =
      getAllHandler userName listId
      :<|> updateHandler userName  listId
      :<|> newHandler userName listId
      :<|> deleteHandler userName listId
      :<|> queryHandler userName listId
      where userName = UsersApi.auName user
    todoHandlers _ _ = SAS.throwAll err401

    checkListAccess listId userName = do
      exists <- DbL.listExists userName listId
      if not exists then throwError (listNotFound listId) else pure ()

    getAllHandler userName listId = do
      checkListAccess listId userName
      Db.listTasks userName listId

    updateHandler userName listId task = do
      checkListAccess listId userName
      liftIO $ putStrLn $ "updating task " ++ show (Db.id task)
      Db.modifyTask userName task
      found <- Db.getTask userName (Db.id task)
      case found of
        Nothing -> throwError notFound
        Just t-> return t

    newHandler userName listId txt = do
      checkListAccess listId userName
      task <- Db.insertTask userName listId txt
      liftIO $ putStrLn $ "created new task - redirecting to " ++ show (Db.id task)
      return task

    deleteHandler userName listId tId = do
      checkListAccess listId userName
      Db.deleteTask userName tId
      liftIO $ putStrLn $ "deleted task " ++ show tId
      Db.listTasks userName listId

    queryHandler userName listId tId = do
      checkListAccess listId userName
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
