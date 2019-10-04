{-# LANGUAGE FlexibleContexts,
             TypeApplications,
             TypeFamilies
#-}

module Db.Internal
  ( Handle (..)
  , useHandle
  , useHandle'
  ) where

import           Control.Concurrent.MVar (MVar, takeMVar, putMVar)
import           Control.Exception (bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ask)
import qualified Database.SQLite.Simple as Sql


newtype Handle =
  Handle (MVar FilePath)

useHandle :: (MonadReader Handle m, MonadIO m) => (Sql.Connection -> IO a) -> m a
useHandle m = do
  handle <- ask
  useHandle' handle m


useHandle' :: MonadIO m => Handle -> (Sql.Connection -> IO a) -> m a
useHandle' (Handle dbFile) m = liftIO $
  bracket
    (takeMVar dbFile)
    (putMVar dbFile)
    (`Sql.withConnection` m)
