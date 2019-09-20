{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module Api.RouteApi
    ( Config (..)
    , RouteApi
    , server
    ) where


import           Data.Aeson ()
import           Data.Aeson.TH ()
import           Data.Text (Text)
import           Page (Config(..))
import qualified Page
import           Servant
import           Servant.HTML.Blaze (HTML)
import           Text.Blaze.Html4.Strict (Markup)


type RouteApi =
  "static" :> Raw
  :<|> CaptureAll "segments" Text :> Get '[HTML] Markup



server :: Config -> Server RouteApi
server config = staticHandler :<|> pageHandler
  where
    pageHandler _ =
      return $ Page.index config

    staticHandler =
      Servant.serveDirectoryWebApp "./static"
