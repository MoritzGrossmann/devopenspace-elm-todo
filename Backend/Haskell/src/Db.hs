{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Db
  ( Handle
  , initDb
  , useHandle
  , DbHandler
  , handleWithContext
  , runActionDb
  , liftDb
  ) where

import           Control.Concurrent.MVar (newMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)
import           Db.Carrier
import           Db.Internal
import qualified Db.Lists as DbLists
import qualified Db.Tasks as DbTasks
import qualified Db.Users as DbUsers


initDb :: MonadIO m => FilePath -> m Handle
initDb file = do
  fileVar <- liftIO $ newMVar file
  let handle = Handle fileVar
  flip runReaderT handle $ do
    DbUsers.createTables
    DbLists.createTables
    DbTasks.createTables
  return handle
