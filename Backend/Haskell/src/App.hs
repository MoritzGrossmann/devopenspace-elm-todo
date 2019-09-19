{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module App
    ( startApp
    , app
    ) where

import qualified Db
import           Network.Wai (Application)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Handler.Warp (run)
import           Servant
import qualified Servant.Auth.Server as SAS
import qualified Api.TodosApi as TodosApi
import           Api.TodosApi (TodosApi)
import qualified Api.UsersApi as UsersApi
import           Api.UsersApi (UsersApi)
import qualified Api.RouteApi as RouteApi
import           Api.RouteApi (RouteApi)

getPort :: IO Int
getPort = return 8080


getDbPath :: IO FilePath
getDbPath = return "./todos.db"


startApp :: IO ()
startApp = do
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
  Servant.serveWithContext (Proxy :: Proxy (UsersApi :<|> TodosApi :<|> RouteApi)) cfg $
    UsersApi.server dbHandle jwtSettings 
    :<|> TodosApi.server dbHandle 
    :<|> RouteApi.server
  where
    myCors = cors $ const $ Just myPolicy
    myPolicy = simpleCorsResourcePolicy { corsMethods = myMethods
                                        , corsRequestHeaders = ["Content-Type"] }
    myMethods = simpleMethods ++ ["PUT", "DELETE", "OPTIONS", "GET"]
