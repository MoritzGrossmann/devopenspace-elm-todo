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
      [ baseUrl ++ "/static/base.css"
      , baseUrl ++ "/static/index.css"
      , baseUrl ++ "/static/app.css"
      ]
    baseUrl = trimEnd siteBaseUrl


scripts :: Config -> H.Html
scripts Config{..} = do
  traverse_ (\ref -> H.script "" ! A.src ref) jsSrcs
  H.script $ H.toHtml $ unlines
    [ "document.startApp({ baseUrlPath: '" ++ siteBaseUrl ++ "', apiUrl: '" ++ apiBaseUrl ++ "' });" ]
  where
    jsSrcs =
      [ fromString $ trimEnd siteBaseUrl ++ "/static/todo.js" ]

trimEnd :: String -> String
trimEnd = reverse . trimStart . reverse

trimStart :: String -> String
trimStart ('/':url) = url
trimStart url = url
