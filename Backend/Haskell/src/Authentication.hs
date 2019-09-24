{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Authentication
    ( Config
    , config
    , newConfig
    , toSettings
    , AuthenticatedUser (..)
    , authCheck
    , hoistServerWithAuth
    ) where


import           Control.Monad.Reader (runReaderT)
import           Crypto.JOSE (JWK)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Swagger.ParamSchema (ToParamSchema)
import           Data.Swagger.Schema (ToSchema)
import qualified Db
import qualified Db.Users as Db
import           GHC.Generics
import qualified Models.User as User
import           Servant
import qualified Servant.Auth as SA
import           Servant.Auth.Server (Auth, ToJWT, FromJWT, AuthResult, FromBasicAuthData, BasicAuthCfg)
import qualified Servant.Auth.Server as SAS

-- Authentication CONFIG

newtype Config = Config
      { jwtKey :: JWK 
      }
$(deriveJSON defaultOptions ''Config)

config :: JWK -> Config
config = Config

newConfig :: IO Config
newConfig = Config <$> SAS.generateKey

toSettings :: Config -> SAS.JWTSettings
toSettings Config {..} = SAS.defaultJWTSettings jwtKey

-- Authenticated User

newtype AuthenticatedUser = AUser
  { auName :: Text
  } deriving (Show, Generic, ToParamSchema, ToSchema)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser


-- Servant helper functions

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

hoistServerWithAuth:: HasServer api '[BasicAuthCfg, SAS.CookieSettings, SAS.JWTSettings]
  => Proxy api
  -> (forall x. m x -> n x)
  -> ServerT api m
  -> ServerT api n
hoistServerWithAuth api =
  hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCfg, SAS.CookieSettings, SAS.JWTSettings])
