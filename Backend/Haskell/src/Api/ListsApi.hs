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


import           GHC.Generics
import           Authentication (AuthenticatedUser (..), hoistServerWithAuth)
import           Control.Effect (runM, LiftC)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Effect.Reader as R
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Swagger.Schema (ToSchema)
import           Data.Text (Text)
import qualified Db
import qualified Db.Lists as Db
import qualified Models.Lists as L
import           Models.User (UserName)
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
    Get '[JSON] [L.List]
    :<|> ReqBody '[JSON] L.List :> Put '[JSON] L.List
    :<|> ReqBody '[JSON] ListName :> Post '[JSON] L.List
    :<|> Capture "id" L.ListId :> Delete '[JSON] [L.List]
    :<|> Capture "id" L.ListId :> Get '[JSON] L.List
  )

type ListHandler a = Db.ListActionDbCarrier (R.ReaderC Db.Handle (LiftC Handler)) a


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
    listsHandlers _ =
      throwErr401
      :<|> (\_ -> throwErr401)
      :<|> (\_ -> throwErr401)
      :<|> (\_ -> throwErr401)
      :<|> (\_ -> throwErr401)

    getAllHandler :: UserName -> ListHandler [L.List]
    getAllHandler =
      L.getAll

    updateHandler :: UserName -> L.List -> ListHandler L.List
    updateHandler userName list = do
      liftIO $ putStrLn $ "updating list " ++ show (L.id list)
      L.modify userName list
      found <- L.getOne userName (L.id list)
      case found of
        Nothing -> liftHandler $ throwError notFound
        Just l -> return l

    newHandler :: UserName -> ListName -> ListHandler L.List
    newHandler userName (ListName txt) = do
      list <- L.create userName txt
      liftIO $ putStrLn $ "created new list - redirecting to " ++ show (L.id list)
      return list

    deleteHandler :: UserName -> L.ListId -> ListHandler [L.List]
    deleteHandler userName lId = do
      L.delete userName lId
      liftIO $ putStrLn $ "deleted list " ++ show lId
      L.getAll userName

    queryHandler :: UserName -> L.ListId -> ListHandler L.List
    queryHandler userName lId = do
      liftIO $ putStrLn $ "getting list " ++ show lId
      found <- L.getOne userName lId
      case found of
        Nothing -> liftHandler $ throwError notFound
        Just list -> return list

    actionToHandler :: ListHandler a -> Handler a
    actionToHandler = runM . R.runReader dbHandle . Db.runListActionDb

    liftHandler :: Handler a -> ListHandler a
    liftHandler = lift . lift . lift

    throwErr401 = liftHandler $ throwError err401

notFound :: ServerError
notFound = err404 { errBody = "sorry - don't know this list" }
