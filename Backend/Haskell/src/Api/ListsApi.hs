{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}


module Api.ListsApi
    ( ListsApi
    , server
    ) where


import           Imports
import           GHC.Generics
import           Authentication (AuthenticatedUser (..), hoistServerWithAuth)
import           Control.Effect (runM, LiftC)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Effect.Reader as R
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Swagger.Schema (ToSchema)
import           Data.Text (Text)
import qualified Db
import qualified Db.Lists as Db
import           Models.Lists (ListAction)
import qualified Models.Lists as L
import           Servant
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth)
import qualified Servant.Auth.Server as SAS

newtype ListName 
  = ListName Text
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type ListsApi =
  Auth '[SA.JWT] AuthenticatedUser :>
  "list" :> (
    Get '[JSON] [Db.List]
    :<|> ReqBody '[JSON] Db.List :> Put '[JSON] Db.List
    :<|> ReqBody '[JSON] ListName :> Post '[JSON] Db.List
    :<|> Capture "id" Db.ListId :> Delete '[JSON] [Db.List]
    :<|> Capture "id" Db.ListId :> Get '[JSON] Db.List
  )

server :: Db.Handle -> Server ListsApi
server dbHandle = hoistServerWithAuth (Proxy :: Proxy ListsApi) actionToHandler listsHandlers
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
      L.getAll

    updateHandler userName list = do
      liftIO $ putStrLn $ "updating list " ++ show (Db.id list)
      L.modify userName list
      found <- L.getOne userName (Db.id list)
      case found of
        Nothing -> lift $ throwError notFound
        Just l -> return l

    newHandler userName (ListName txt) = do
      list <- L.create userName txt
      liftIO $ putStrLn $ "created new list - redirecting to " ++ show (Db.id list)
      return list

    deleteHandler userName lId = do
      L.delete userName lId
      liftIO $ putStrLn $ "deleted list " ++ show lId
      L.getAll userName

    queryHandler userName lId = do
      liftIO $ putStrLn $ "getting list " ++ show lId
      found <- L.getOne userName lId
      case found of
        Nothing -> throwError notFound
        Just list -> return list

    toHandle :: ReaderT Db.Handle Handler a -> Handler a
    toHandle r = runReaderT r dbHandle

    actionToHandler :: Db.ListActionDbCarrier (R.ReaderC Db.Handle (LiftC Handler)) a -> Handler a
    actionToHandler = runM . R.runReader dbHandle . Db.runListActionDb

notFound :: ServerError
notFound = err404 { errBody = "sorry - don't know this list" }
