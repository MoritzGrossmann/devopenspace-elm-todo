{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}


module App
    ( startApp
    , app
    ) where

import           Api.ListsApi (ListsApi)
import qualified Api.ListsApi as ListsApi
import           Api.RouteApi (RouteApi)
import qualified Api.RouteApi as RouteApi
import           Api.TodosApi (TodosApi)
import qualified Api.TodosApi as TodosApi
import           Api.UsersApi (UsersApi)
import qualified Api.UsersApi as UsersApi
import qualified Authentication as Auth
import           Context
import qualified Db
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors
import qualified Page
import           Servant hiding (Context)
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

  let context = Context dbHandle (Auth.toSettings authConfig)

  putStrLn $ "starting Server on " ++ show serverPort
  run serverPort $ app context (toPageConfig settings)


-- prefix APIs with "api/"
type AuthApiProxy = UsersApi :<|> ListsApi :<|> TodosApi
type ApiProxy = "api" :> AuthApiProxy
type WebProxy = RouteApi
type API = SwUI.SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> ApiProxy :<|> WebProxy


app :: Context -> Page.Config -> Application
app context pageConfig = myCors $ do
  let dbHandle = contextDbHandle context
      jwtSettings = contextJwtSettings context
      authCfg = Auth.authCheck dbHandle
      cfg = authCfg :. SAS.defaultCookieSettings :. jwtSettings :. EmptyContext
      authServer = Auth.hoistServerWithAuth
        (Proxy @AuthApiProxy)
        (Db.handleWithContext context)
        (UsersApi.serverT :<|> ListsApi.serverT :<|> TodosApi.serverT)
  Servant.serveWithContext (Proxy :: Proxy API) cfg $
    SwUI.swaggerSchemaUIServer swaggerDoc
    :<|> authServer
    :<|> RouteApi.server pageConfig
  where
    myCors = cors $ const $ Just myPolicy
    myPolicy = simpleCorsResourcePolicy { corsMethods = myMethods
                                        , corsRequestHeaders = ["Content-Type", "authorization"] }
    myMethods = simpleMethods ++ ["PUT", "DELETE", "OPTIONS", "GET"]
    swaggerDoc = Sw.toSwagger (Proxy :: Proxy ApiProxy)
