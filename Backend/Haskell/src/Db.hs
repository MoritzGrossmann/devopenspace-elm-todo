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
import qualified Db.Users as DbUsers


initDb :: MonadIO m => FilePath -> m Handle
initDb file = do
  fileVar <- liftIO $ newMVar file
  let handle = Handle fileVar
  flip runReaderT handle $ do
    DbUsers.createTables
    DbTasks.createTables
  return handle