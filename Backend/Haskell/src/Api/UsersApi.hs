{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}

module Api.UsersApi
    ( UsersApi
    , AuthenticatedUser (..)
    , server
    , authCheck
    , hoistServerWithAuth
    ) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Db
import qualified Db.Users as Db
import           GHC.Generics
import qualified Models.User as User
import           Servant
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth, ToJWT, FromJWT, AuthResult, FromBasicAuthData, BasicAuthCfg)
import qualified Servant.Auth.Server as SAS
import           Servant.Docs (ToSample(..), singleSample, HasDocs(..))

newtype AuthenticatedUser = AUser
  { auName :: Text
  } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

instance ToSample AuthenticatedUser where
  toSamples _ = singleSample $ AUser "todoUser"

instance HasDocs rest => HasDocs (Auth '[SA.BasicAuth] AuthenticatedUser :> rest) where
  docsFor _ (endpoint, action) opts = docsFor (Proxy :: Proxy rest) (endpoint, action) opts

instance HasDocs rest => HasDocs (Auth '[SA.JWT] AuthenticatedUser :> rest) where
  docsFor _ (endpoint, action) opts = docsFor (Proxy :: Proxy rest) (endpoint, action) opts

type UsersApi =
  "user" :> (
    "register" :> ReqBody '[JSON] User.Login :> Post '[JSON] Text
    :<|> Auth '[SA.BasicAuth] AuthenticatedUser :> "login" :> Get '[JSON] Text
  )

authCheck :: Db.Handle -> BasicAuthData -> IO (AuthResult AuthenticatedUser)
authCheck dbHandle (BasicAuthData login password) = flip runReaderT dbHandle $ do
  userRes <- Db.getUser (decodeUtf8 login)
  case userRes of
    Nothing -> pure SAS.NoSuchUser
    Just user
      | User.validatePassword user (decodeUtf8 password) ->
        pure $ SAS.Authenticated (AUser $ User.userName user)
      | otherwise ->
        pure SAS.BadPassword

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

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

hoistServerWithAuth
  :: HasServer api '[BasicAuthCfg, SAS.CookieSettings, SAS.JWTSettings]
  => Proxy api
  -> (forall x. m x -> n x)
  -> ServerT api m
  -> ServerT api n
hoistServerWithAuth api =
  hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCfg, SAS.CookieSettings, SAS.JWTSettings])
