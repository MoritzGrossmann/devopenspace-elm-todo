{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module Api.ListsApi
    ( ListsApi
    , server
    ) where


import           Authentication (AuthenticatedUser (..), hoistServerWithAuth)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Text (Text)
import qualified Db
import qualified Db.Lists as Db
import           Servant
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth)
import qualified Servant.Auth.Server as SAS
import           Servant.Docs


type ListsApi =
  Auth '[SA.JWT] AuthenticatedUser :>
  "list" :> (
    Get '[JSON] [Db.List]
    :<|> ReqBody '[JSON] Db.List :> Put '[JSON] Db.List
    :<|> ReqBody '[JSON] Text :> Post '[JSON] Db.List
    :<|> Capture "id" Db.ListId :> Delete '[JSON] [Db.List]
    :<|> Capture "id" Db.ListId :> Get '[JSON] Db.List
  )

instance ToCapture (Capture "id" Db.ListId) where
  toCapture _ = DocCapture "id" "ID der Liste die benutzt werden soll"

server :: Db.Handle -> Server ListsApi
server dbHandle = hoistServerWithAuth (Proxy :: Proxy ListsApi) toHandle listsHandlers
  where
    listsHandlers (SAS.Authenticated user) =
      getAllHandler userName
      :<|> updateHandler userName 
      :<|> newHandler userName
      :<|> deleteHandler userName
      :<|> queryHandler userName
      where userName = auName user
    listsHandlers _ = SAS.throwAll err401

    getAllHandler =
      Db.listLists

    updateHandler userName list = do
      liftIO $ putStrLn $ "updating list " ++ show (Db.id list)
      Db.modifyList userName list
      found <- Db.getList userName (Db.id list)
      case found of
        Nothing -> throwError notFound
        Just l -> return l

    newHandler userName txt = do
      list <- Db.insertList userName txt
      liftIO $ putStrLn $ "created new list - redirecting to " ++ show (Db.id list)
      return list

    deleteHandler userName lId = do
      Db.deleteList userName lId
      liftIO $ putStrLn $ "deleted list " ++ show lId
      Db.listLists userName

    queryHandler userName lId = do
      liftIO $ putStrLn $ "getting list " ++ show lId
      found <- Db.getList userName lId
      case found of
        Nothing -> throwError notFound
        Just list -> return list

    toHandle :: ReaderT Db.Handle Handler a -> Handler a
    toHandle r = runReaderT r dbHandle


notFound :: ServerError
notFound = err404 { errBody = "sorry - don't know this list" }
