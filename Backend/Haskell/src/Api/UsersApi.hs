{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.UsersApi
    ( UsersApi
    , server
    ) where


import           Authentication
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Db
import qualified Db.Users as Db
import qualified Models.User as User
import           Servant
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth)
import qualified Servant.Auth.Server as SAS

type UsersApi =
  "user" :> (
    "register" :> ReqBody '[JSON] User.Login :> Post '[JSON] Text
    :<|> Auth '[SA.BasicAuth] AuthenticatedUser :> "login" :> Get '[JSON] Text
  )

server :: Db.Handle -> SAS.JWTSettings -> Server UsersApi
server dbHandle jwtSettings = hoistServerWithAuth (Proxy :: Proxy UsersApi) toHandle userHandlers
  where
    userHandlers =
      registerHandler
      :<|> loginHandler

    registerHandler login = do
      userRes <- liftIO $ User.create login
      case userRes of
        Nothing -> SAS.throwAll err500
        Just user -> do
          liftIO $ putStrLn $ "registering user " ++ show user
          Db.insertUser user
          returnJwtFor (AUser $ User.userName user)

    loginHandler (SAS.Authenticated user) = do
      liftIO $ putStrLn $ "user " ++ show user ++ " logged in"
      returnJwtFor user
    loginHandler _ = SAS.throwAll err401

    returnJwtFor user = do
      jwtRes <- liftIO $ SAS.makeJWT user jwtSettings Nothing
      case jwtRes of
        Left _ -> SAS.throwAll err500
        Right jwt -> pure $ decodeUtf8 $ BSL.toStrict jwt

    toHandle :: ReaderT Db.Handle Handler a -> Handler a
    toHandle r = runReaderT r dbHandle