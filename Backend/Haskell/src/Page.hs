{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Page
  ( Config (..)
  , index
  ) where

import           Data.Foldable (traverse_)
import           Data.String (fromString)
import           Text.Blaze.Html4.Strict (Markup, (!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

data Config = Config
  { siteBaseUrl :: String
  , apiBaseUrl :: String
  }

index :: Config -> Markup
index config = H.html $ do
  H.head $ do
    H.title "Todo in Elm"
    links config
  H.body $ scripts config


links :: Config -> H.Html
links Config{..} =
  traverse_ (\ref -> H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href ref) cssSrcs
  where
    cssSrcs = fmap fromString
      [ siteBaseUrl ++ "static/base.css"
      , siteBaseUrl ++ "static/index.css"
      , siteBaseUrl ++ "static/style.css"
      ]


scripts :: Config -> H.Html
scripts Config{..} = do
  traverse_ (\ref -> H.script "" ! A.src ref) jsSrcs
  H.script $ H.toHtml $ unlines
    [ "var app = Elm.Main.init({ flags: { baseUrlPath: '" ++ siteBaseUrl ++ "', apiUrl: '" ++ apiBaseUrl ++ "' } });"
    , "window.initPorts(app);"
    ]
  where
    jsSrcs =
      [ fromString $ siteBaseUrl ++ "static/ports.js"
      , fromString $ siteBaseUrl ++ "static/todo.js"
      ]
