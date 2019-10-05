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
import           Models.User (Login, ChangePassword)
import qualified Models.User as User
import qualified Models.User.Effects as User
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
    "register" :> ReqBody '[JSON] Login :> Post '[JSON] JwtToken
    :<|> Auth '[SA.BasicAuth, SA.JWT] AuthenticatedUser :> "login" :> Get '[JSON] JwtToken
    :<|> Auth '[SA.BasicAuth, SA.JWT] AuthenticatedUser :> "changePwd" :> ReqBody '[JSON] ChangePassword :> Post '[JSON] NoContent
  )

serverT :: ServerT UsersApi DbHandler
serverT = userHandlers
  where
    userHandlers =
      registerHandler
      :<|> loginHandler
      :<|> changePwdHandler

    registerHandler :: Login -> DbHandler JwtToken
    registerHandler login = do
      registerRes <- User.register login
      case registerRes of
        Nothing -> liftHandler $ throwError err500
        Just user -> do
          liftIO $ putStrLn $ "registering user " ++ show user
          returnJwtFor (AUser $ User.userName user)

    loginHandler :: SAS.AuthResult AuthenticatedUser -> DbHandler JwtToken
    loginHandler (SAS.Authenticated user) = do
      liftIO $ putStrLn $ "user " ++ show user ++ " logged in"
      returnJwtFor user
    loginHandler _ = liftHandler $ throwError err401

    changePwdHandler :: (SAS.AuthResult AuthenticatedUser) -> ChangePassword -> DbHandler NoContent
    changePwdHandler (SAS.Authenticated authUser) pwdChange = do
      let userName = auName authUser
      changeRes <- User.changePassword userName pwdChange
      case changeRes of
        User.ChPwdSuccess -> do
          liftIO $ putStrLn $ "user " ++ show userName ++ " changed password"
          pure NoContent
        User.ChPwdInvalidUser -> do
          liftIO $ putStrLn $ "failed to change password for user " ++ show userName ++ ": user not found"
          liftHandler $ throwError err400
        User.ChPwdInvalidPassword -> do
          liftIO $ putStrLn $ "failed to change password for user " ++ show userName ++ ": invalid password"
          liftHandler $ throwError err400
        User.ChPwdInternalError -> do
          liftIO $ putStrLn $ "failed to change password for user " ++ show userName ++ ": could not hash password"
          liftHandler $ throwError err500
    changePwdHandler _ _ = liftHandler $ throwError err401

    returnJwtFor :: AuthenticatedUser -> DbHandler JwtToken
    returnJwtFor user = do
      jwtSettings <- getJwtSettings
      jwtRes <- liftIO $ SAS.makeJWT user jwtSettings Nothing
      case jwtRes of
        Left _ -> liftHandler $ throwError err500
        Right jwt -> pure $ JwtToken $ decodeUtf8 $ BSL.toStrict jwt
