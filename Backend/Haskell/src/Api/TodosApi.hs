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
    todoHandlers (SAS.Authenticated _) =
      getAllHandler :<|> updateHandler :<|> newHandler :<|> deleteHandler :<|> queryHandler
    todoHandlers _ = SAS.throwAll err401

    getAllHandler =
      Db.listTasks

    updateHandler task = do
      liftIO $ putStrLn $ "updating task " ++ show (Db.id task)
      Db.modifyTask task
      found <- Db.getTask (Db.id task)
      case found of
        Nothing -> throwError notFound
        Just t-> return t

    newHandler txt = do
      task <- Db.insertTask txt
      liftIO $ putStrLn $ "created new task - redirecting to " ++ show (Db.id task)
      return task

    deleteHandler tId = do
      Db.deleteTask tId
      liftIO $ putStrLn $ "deleted task " ++ show tId
      Db.listTasks

    queryHandler tId = do
      liftIO $ putStrLn $ "getting task " ++ show tId
      found <- Db.getTask tId
      case found of
        Nothing -> throwError notFound
        Just task -> return task

    toHandle :: ReaderT Db.Handle Handler a -> Handler a
    toHandle r = runReaderT r dbHandle


notFound :: ServerError
notFound = err404 { errBody = "sorry - don't know this task" }
