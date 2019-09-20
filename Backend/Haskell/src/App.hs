{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}


module App
    ( startApp
    , app
    , writeDocs
    ) where

import qualified Authentication as Auth
import           Api.ListsApi (ListsApi)
import qualified Api.ListsApi as ListsApi
import           Api.RouteApi (RouteApi)
import qualified Api.RouteApi as RouteApi
import           Api.TodosApi (TodosApi)
import qualified Api.TodosApi as TodosApi
import           Api.UsersApi (UsersApi)
import qualified Api.UsersApi as UsersApi
import           Data.Text (Text)
import qualified Db
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors
import           Servant
import qualified Servant.Auth.Server as SAS
import           Servant.Docs (ToSample(..), singleSample)
import qualified Servant.Docs as Docs
import           Settings (Settings(..), loadSettings)

startApp :: FilePath -> IO ()
startApp settingsPath = do
  Settings{..} <- loadSettings settingsPath

  writeDocs "docs.md"
  putStrLn $ "initializing Database in " ++ databasePath
  dbHandle <- Db.initDb databasePath

  putStrLn $ "starting Server on " ++ show serverPort
  run serverPort $ app dbHandle (Auth.toSettings authConfig)


-- prefix APIs with "api/"
type ApiProxy = "api" :> (UsersApi :<|> ListsApi :<|> TodosApi)
type WebProxy = RouteApi

app :: Db.Handle -> SAS.JWTSettings -> Application
app dbHandle jwtSettings = myCors $ do
  let authCfg = Auth.authCheck dbHandle
      cfg = authCfg :. SAS.defaultCookieSettings :. jwtSettings :. EmptyContext
  Servant.serveWithContext (Proxy :: Proxy (ApiProxy :<|> WebProxy)) cfg $
    (UsersApi.server dbHandle jwtSettings
    :<|> ListsApi.server dbHandle
    :<|> TodosApi.server dbHandle)
    :<|> RouteApi.server
  where
    myCors = cors $ const $ Just myPolicy
    myPolicy = simpleCorsResourcePolicy { corsMethods = myMethods
                                        , corsRequestHeaders = ["Content-Type", "authorization"] }
    myMethods = simpleMethods ++ ["PUT", "DELETE", "OPTIONS", "GET"]

writeDocs :: FilePath -> IO ()
writeDocs outputFile = do
  let doc = Docs.markdown $ Docs.docs (Proxy :: Proxy (UsersApi :<|> ListsApi :<|> TodosApi))
  writeFile outputFile doc

instance ToSample Text where
  toSamples _ = singleSample "Text"
