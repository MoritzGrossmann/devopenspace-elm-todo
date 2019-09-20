{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}


module Settings
    ( Settings (..)
    , defaultSettings
    , loadSettings
    , saveSettings
    ) where

import qualified Authentication as Auth
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import           Data.Yaml (encodeFile, decodeFileEither)

data Settings = Settings
  { serverPort   :: Int
  , databasePath :: FilePath
  , authConfig   :: Auth.Config
  }

$(deriveJSON defaultOptions ''Settings)

defaultSettings :: IO Settings
defaultSettings = 
  Settings 8080 "./todos.db" <$> Auth.newConfig

loadSettings :: FilePath -> IO Settings
loadSettings settingsPath = do
  res <- decodeFileEither settingsPath
  either fallback pure res
  where fallback _ = do
          def <- defaultSettings
          saveSettings def settingsPath
          pure def

saveSettings :: Settings -> FilePath -> IO ()
saveSettings settings settingsPath =
  encodeFile settingsPath settings