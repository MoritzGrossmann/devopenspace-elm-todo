{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards   #-}


module Settings
    ( Settings (..)
    , defaultSettings
    , loadSettings
    , saveSettings
    , toPageConfig
    ) where

import qualified Authentication as Auth
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.Yaml (encodeFile, decodeFileEither)
import qualified Page as Page

data Settings = Settings
  { serverPort   :: Int
  , databasePath :: FilePath
  , siteBaseUrl  :: String
  , apiBaseUrl   :: String
  , authConfig   :: Auth.Config
  }

$(deriveJSON defaultOptions ''Settings)

defaultSettings :: IO Settings
defaultSettings =
  Settings 8080 "./todos.db" "/" "/api" <$> Auth.newConfig

loadSettings :: FilePath -> IO Settings
loadSettings settingsPath = do
  res <- decodeFileEither settingsPath
  either fallback success res
  where fallback _ = do
          putStrLn $ "could not load " ++ settingsPath ++  " falling back to default.."
          def <- defaultSettings
          saveSettings def settingsPath
          pure def
        success settings = do
          putStrLn $ "successfully loaded " ++ settingsPath ++ "\n"
            ++ "db-path: " ++ databasePath settings
            ++ "\nserverPort: " ++ show (serverPort settings) ++ "\n"
          pure settings

saveSettings :: Settings -> FilePath -> IO ()
saveSettings settings settingsPath =
  encodeFile settingsPath settings

toPageConfig :: Settings -> Page.Config
toPageConfig Settings{..} =
  Page.Config siteBaseUrl apiBaseUrl
