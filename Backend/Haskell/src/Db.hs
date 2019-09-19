{-# LANGUAGE FlexibleContexts #-}

module Db
  ( Handle
  , initDb
  ) where

import           Control.Concurrent.MVar (newMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)
import           Db.Internal
import qualified Db.Tasks as DbTasks


initDb :: MonadIO m => FilePath -> m Handle
initDb file = do
  fileVar <- liftIO $ newMVar file
  let handle = Handle fileVar
  runReaderT DbTasks.createTables handle
  return handle