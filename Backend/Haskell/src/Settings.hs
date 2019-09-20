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
