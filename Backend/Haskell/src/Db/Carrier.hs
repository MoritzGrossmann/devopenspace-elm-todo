{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Db.Carrier
  ( DbHandler
  , ActionDbCarrier (..)
  , handleWithContext
  , runActionDb
  , liftDb
  , useContext
  ) where

import           Context.Internal
import           Control.Effect.Lift (LiftC, runM)
import           Control.Effect.Reader (ReaderC, Reader)
import qualified Control.Effect.Reader as R
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Database.SQLite.Simple as Sql
import           Db.Internal
import           Imports
import           Servant (Handler)

useContext :: (MonadReader Context m, MonadIO m) => (Sql.Connection -> IO a) -> m a
useContext m = do
  handle <- asks contextDbHandle
  useHandle' handle m


liftDb :: (Has (Reader Context) sig m, MonadIO m, m' ~ ReaderT Handle IO)
       => m' a -> m a
liftDb m' = do
  handle <- R.asks contextDbHandle
  liftIO $ runReaderT m' handle


type DbHandler = ActionDbCarrier (ReaderC Context (LiftC Handler))

handleWithContext :: Context -> DbHandler a -> Handler a
handleWithContext context = runM . R.runReader context . runActionDb

newtype ActionDbCarrier m a
  = ActionDbCarrier { runActionDbCarrier :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans ActionDbCarrier where
  lift m = ActionDbCarrier m

runActionDb :: ActionDbCarrier m a -> m a
runActionDb = runActionDbCarrier
