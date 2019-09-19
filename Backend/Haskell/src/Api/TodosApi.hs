{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
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
import           Data.Text (Text)
import qualified Db
import qualified Db.Tasks as Db
import           Servant
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth)
import qualified Servant.Auth.Server as SAS


type TodosApi =
  "todos" :> Auth '[SA.JWT] AuthenticatedUser :> (
    Get '[JSON] [Db.Task]
    :<|> ReqBody '[JSON] Db.Task :> Put '[JSON] Db.Task
    :<|> ReqBody '[JSON] Text :> Post '[JSON] Db.Task
    :<|> Capture "id" Db.TaskId :> Delete '[JSON] [Db.Task]
    :<|> Capture "id" Db.TaskId :> Get '[JSON] Db.Task
  )


server :: Db.Handle -> Server TodosApi
server dbHandle = UsersApi.hoistServerWithAuth (Proxy :: Proxy TodosApi) toHandle todoHandlers
  where
    todoHandlers (SAS.Authenticated user) =
      getAllHandler userName 
      :<|> updateHandler userName 
      :<|> newHandler userName 
      :<|> deleteHandler userName 
      :<|> queryHandler userName
      where userName = UsersApi.auName user
    todoHandlers _ = SAS.throwAll err401

    getAllHandler =
      Db.listTasks

    updateHandler userName task = do
      liftIO $ putStrLn $ "updating task " ++ show (Db.id task)
      Db.modifyTask userName task
      found <- Db.getTask userName (Db.id task)
      case found of
        Nothing -> throwError notFound
        Just t-> return t

    newHandler userName txt = do
      task <- Db.insertTask userName txt
      liftIO $ putStrLn $ "created new task - redirecting to " ++ show (Db.id task)
      return task

    deleteHandler userName tId = do
      Db.deleteTask userName tId
      liftIO $ putStrLn $ "deleted task " ++ show tId
      Db.listTasks userName

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
