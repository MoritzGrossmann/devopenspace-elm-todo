{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}


module App
    ( startApp
    , app
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
import qualified Db
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors
import qualified Page
import           Servant
import qualified Servant.Auth.Server as SAS
import           Servant.Auth.Swagger ()
import qualified Servant.Swagger as Sw
import qualified Servant.Swagger.UI as SwUI
import           Settings (Settings(..), loadSettings, toPageConfig)

startApp :: FilePath -> IO ()
startApp settingsPath = do
  settings@Settings{..} <- loadSettings settingsPath

  -- writeDocs "docs.md"
  putStrLn $ "initializing Database in " ++ databasePath
  dbHandle <- Db.initDb databasePath

  putStrLn $ "starting Server on " ++ show serverPort
  run serverPort $ app dbHandle (toPageConfig settings) (Auth.toSettings authConfig)


-- prefix APIs with "api/"
type ApiProxy = "api" :> (UsersApi :<|> ListsApi :<|> TodosApi)
type WebProxy = RouteApi
type API = SwUI.SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> ApiProxy :<|> WebProxy


app :: Db.Handle -> Page.Config -> SAS.JWTSettings -> Application
app dbHandle pageConfig jwtSettings = myCors $ do
  let authCfg = Auth.authCheck dbHandle
      cfg = authCfg :. SAS.defaultCookieSettings :. jwtSettings :. EmptyContext
  Servant.serveWithContext (Proxy :: Proxy API) cfg $
    SwUI.swaggerSchemaUIServer swaggerDoc
    :<|> (UsersApi.server dbHandle jwtSettings
    :<|> (Auth.hoistServerWithAuth (Proxy :: Proxy ListsApi) (Db.handleWithDb dbHandle) ListsApi.serverT)
    :<|> TodosApi.server dbHandle)
    :<|> RouteApi.server pageConfig
  where
    myCors = cors $ const $ Just myPolicy
    myPolicy = simpleCorsResourcePolicy { corsMethods = myMethods
                                        , corsRequestHeaders = ["Content-Type", "authorization"] }
    myMethods = simpleMethods ++ ["PUT", "DELETE", "OPTIONS", "GET"]
    swaggerDoc = Sw.toSwagger (Proxy :: Proxy ApiProxy)
