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
    , serverT
    ) where


import           Authentication
import           Context (getJwtSettings)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as BSL
import           Data.Swagger.ParamSchema (ToParamSchema)
import           Data.Swagger.Schema (ToSchema)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Db (DbHandler, liftHandler)
import qualified Models.User as User
import           Servant
import           Servant.Auth (Auth)
import qualified Servant.Auth as SA
import qualified Servant.Auth.Server as SAS

newtype JwtToken
  = JwtToken Text
  deriving (ToJSON, ToSchema, ToParamSchema)

instance ToSchema NoContent

type UsersApi =
  "user" :> (
    "register" :> ReqBody '[JSON] User.Login :> Post '[JSON] JwtToken
    :<|> Auth '[SA.BasicAuth] AuthenticatedUser :> "login" :> Get '[JSON] JwtToken
    :<|> Auth '[SA.BasicAuth] AuthenticatedUser :> "changePwd" :> ReqBody '[JSON] User.ChangePassword :> Post '[JSON] NoContent
  )

serverT :: ServerT UsersApi DbHandler
serverT = userHandlers
  where
    userHandlers =
      registerHandler
      :<|> loginHandler
      :<|> changePwdHandler

    registerHandler :: User.Login -> DbHandler JwtToken
    registerHandler login = do
      userRes <- User.create login
      case userRes of
        Nothing -> liftHandler $ throwError err500
        Just user -> do
          liftIO $ putStrLn $ "registering user " ++ show user
          returnJwtFor (AUser $ User.userName user)

    loginHandler :: SAS.AuthResult AuthenticatedUser -> DbHandler JwtToken
    loginHandler (SAS.Authenticated user) = do
      liftIO $ putStrLn $ "user " ++ show user ++ " logged in"
      returnJwtFor user
    loginHandler _ = liftHandler $ throwError err401

    changePwdHandler :: (SAS.AuthResult AuthenticatedUser) -> User.ChangePassword -> DbHandler NoContent
    changePwdHandler (SAS.Authenticated authUser) User.ChangePassword{..} = do
      let userName = auName authUser
      foundUser <- User.get userName
      case foundUser of
        Just user | User.validatePassword user oldPassword -> do
          createRes <- liftIO $ User.createIO (User.Login userName newPassword)
          case createRes of
            Just changedUser -> do
              User.update userName changedUser
              liftIO $ putStrLn $ "user " ++ show user ++ " changed password"
              pure NoContent
            Nothing -> liftHandler $ throwError err500
        _ -> liftHandler $ throwError err500
    changePwdHandler _ _ = liftHandler $ throwError err401

    returnJwtFor :: AuthenticatedUser -> DbHandler JwtToken
    returnJwtFor user = do
      jwtSettings <- getJwtSettings
      jwtRes <- liftIO $ SAS.makeJWT user jwtSettings Nothing
      case jwtRes of
        Left _ -> liftHandler $ throwError err500
        Right jwt -> pure $ JwtToken $ decodeUtf8 $ BSL.toStrict jwt
