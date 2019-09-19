{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module Api.TodosApi
    ( TodosApi
    , server
    ) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Text (Text)
import qualified Db
import qualified Db.Tasks as Db
import           Servant
import           Servant.API.WebSocket (WebSocket)
import qualified Ws


type TodosApi =
  "todos" :> (
    Get '[JSON] [Db.Task]
    :<|> ReqBody '[JSON] Db.Task :> Put '[JSON] Db.Task
    :<|> ReqBody '[JSON] Text :> Post '[JSON] Db.Task
    :<|> Capture "id" Db.TaskId :> Delete '[JSON] [Db.Task]
    :<|> Capture "id" Db.TaskId :> Get '[JSON] Db.Task
    :<|> "listen" :> WebSocket
  )


server :: Db.Handle -> Ws.Handle -> Server TodosApi
server dbHandle wsHandle = hoistServer (Proxy :: Proxy TodosApi) toHandle todoHandlers
  where
    todoHandlers =
      getAllHandler :<|> updateHandler :<|> newHandler :<|> deleteHandler :<|> queryHandler :<|> wsHandler

    getAllHandler =
      Db.listTasks

    updateHandler task = do
      liftIO $ putStrLn $ "updating task " ++ show (Db.id task)
      Db.modifyTask task
      Ws.broadcast wsHandle (Ws.UpdateTask task)
      found <- Db.getTask (Db.id task)
      case found of
        Nothing -> throwError notFound
        Just t-> return t

    newHandler txt = do
      task <- Db.insertTask txt
      liftIO $ putStrLn $ "created new task - redirecting to " ++ show (Db.id task)
      Ws.broadcast wsHandle (Ws.NewTask task)
      return task

    deleteHandler tId = do
      Db.deleteTask tId
      liftIO $ putStrLn $ "deleted task " ++ show tId
      Ws.broadcast wsHandle (Ws.DeleteTask tId)
      Db.listTasks

    queryHandler tId = do
      liftIO $ putStrLn $ "getting task " ++ show tId
      found <- Db.getTask tId
      case found of
        Nothing -> throwError notFound
        Just task -> return task

    wsHandler =
      Ws.connect wsHandle

    toHandle :: ReaderT Db.Handle Handler a -> Handler a
    toHandle r = runReaderT r dbHandle


notFound :: ServantErr
notFound = err404 { errBody = "sorry - don't know this task" }
