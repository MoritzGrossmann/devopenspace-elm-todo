{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module App
    ( startApp
    , app
    , writeDocs
    ) where

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

getPort :: IO Int
getPort = return 8080


getDbPath :: IO FilePath
getDbPath = return "./todos.db"


startApp :: IO ()
startApp = do
  writeDocs "docs.md"
  dbPath <- getDbPath
  putStrLn $ "initializing Database in " ++ dbPath
  dbHandle <- Db.initDb dbPath

  port <- getPort
  putStrLn $ "starting Server on " ++ show port

  myKey <- SAS.generateKey
  let jwtSettings = SAS.defaultJWTSettings myKey

  run port $ app dbHandle jwtSettings


app :: Db.Handle -> SAS.JWTSettings -> Application
app dbHandle jwtSettings = myCors $ do
  let authCfg = UsersApi.authCheck dbHandle
      cfg = authCfg :. SAS.defaultCookieSettings :. jwtSettings :. EmptyContext
  Servant.serveWithContext (Proxy :: Proxy (UsersApi :<|> ListsApi :<|> TodosApi :<|> RouteApi)) cfg $
    UsersApi.server dbHandle jwtSettings
    :<|> ListsApi.server dbHandle
    :<|> TodosApi.server dbHandle
    :<|> RouteApi.server
  where
    myCors = cors $ const $ Just myPolicy
    myPolicy = simpleCorsResourcePolicy { corsMethods = myMethods
                                        , corsRequestHeaders = ["Content-Type"] }
    myMethods = simpleMethods ++ ["PUT", "DELETE", "OPTIONS", "GET"]

writeDocs :: FilePath -> IO ()
writeDocs outputFile = do
  let doc = Docs.markdown $ Docs.docs (Proxy :: Proxy (UsersApi :<|> ListsApi :<|> TodosApi))
  writeFile outputFile doc

instance ToSample Text where
  toSamples _ = singleSample "Text"
