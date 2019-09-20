module Main where

import App (startApp)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "using local settings file"
      startApp "./settings.yaml"
    settingsPath:_ -> do
      putStrLn $ "using " ++ settingsPath
      startApp settingsPath
