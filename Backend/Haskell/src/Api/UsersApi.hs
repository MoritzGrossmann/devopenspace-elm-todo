{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api.UsersApi
    ( UsersApi
    , server
    ) where


import           Authentication
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Swagger.Schema (ToSchema)
import           Data.Swagger.ParamSchema (ToParamSchema)
import qualified Db
import qualified Db.Users as Db
import qualified Models.User as User
import           Servant
import           Servant.Auth (Auth)
import qualified Servant.Auth as SA
import qualified Servant.Auth.Server as SAS
import           Servant.Docs (ToSample(..), singleSample)

newtype JwtToken 
  = JwtToken Text
  deriving (ToJSON, ToSchema, ToParamSchema)

instance ToSample JwtToken where
  toSamples _ = singleSample $ 
     JwtToken "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXVOYW1lIjoiQmF0bWFuIn19.ooUdce2IFf5py1UK6vzSpSaH8APECzl0Gp8V5I7-6OIk-vgXtroQpYfkhLH2gmq0aPIeTS5v0fjlRAx-uwZCaA"

instance ToSchema NoContent

type UsersApi =
  "user" :> (
    "register" :> ReqBody '[JSON] User.Login :> Post '[JSON] JwtToken
    :<|> Auth '[SA.BasicAuth] AuthenticatedUser :> "login" :> Get '[JSON] JwtToken
    :<|> Auth '[SA.BasicAuth] AuthenticatedUser :> "changePwd" :> ReqBody '[JSON] User.ChangePassword :> Post '[JSON] NoContent
  )

server :: Db.Handle -> SAS.JWTSettings -> Server UsersApi
server dbHandle jwtSettings = hoistServerWithAuth (Proxy :: Proxy UsersApi) toHandle userHandlers
  where
    userHandlers =
      registerHandler
      :<|> loginHandler
      :<|> changePwdHandler

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

    changePwdHandler (SAS.Authenticated authUser) User.ChangePassword{..} = do
      let userName = auName authUser
      foundUser <- Db.getUser userName
      case foundUser of
        Just user | User.validatePassword user oldPassword -> do
          createRes <- User.create (User.Login userName newPassword)
          case createRes of
            Just changedUser -> do
              Db.updateUser userName changedUser
              liftIO $ putStrLn $ "user " ++ show user ++ " changed password"
              pure NoContent
            Nothing -> SAS.throwAll err500
        _ -> SAS.throwAll err500
    changePwdHandler _ _ = SAS.throwAll err401

    returnJwtFor user = do
      jwtRes <- liftIO $ SAS.makeJWT user jwtSettings Nothing
      case jwtRes of
        Left _ -> SAS.throwAll err500
        Right jwt -> pure $ JwtToken $ decodeUtf8 $ BSL.toStrict jwt

    toHandle :: ReaderT Db.Handle Handler a -> Handler a
    toHandle r = runReaderT r dbHandle