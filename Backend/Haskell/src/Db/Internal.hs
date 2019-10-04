{-# LANGUAGE FlexibleContexts,
             TypeApplications,
             TypeFamilies
#-}

module Db.Internal
  ( Handle (..)
  , useHandle
  , useHandle'
  , liftDb
  ) where

import           Imports
import           Control.Concurrent.MVar (MVar, takeMVar, putMVar)
import           Control.Effect.Reader (Reader)
import qualified Control.Effect.Reader as R
import           Control.Exception (bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import qualified Database.SQLite.Simple as Sql


newtype Handle = 
  Handle (MVar FilePath)

liftDb :: (Has (Reader Handle) sig m, MonadIO m, m' ~ ReaderT Handle IO) 
       => m' a -> m a
liftDb m' = do
  handle <- R.ask @Handle
  liftIO $ runReaderT m' handle


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