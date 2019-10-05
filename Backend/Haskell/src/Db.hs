{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Db
  ( Handle
  , initDb
  , useHandle
  , DbHandler
  , handleWithContext
  , liftDb
  , liftHandler
  ) where

import           Context.Internal
import           Control.Concurrent.MVar (newMVar)
import           Control.Effect.Lift (LiftC, runM)
import           Control.Effect.Reader (ReaderC, runReader)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Db.Carrier (ActionDbCarrier, liftDb, runActionDb)
import           Db.Internal
import qualified Db.Lists as DbLists
import qualified Db.Tasks as DbTasks
import qualified Db.Users as DbUsers
import qualified Db.Users.Carrier as DbUsers
import           Imports


initDb :: MonadIO m => FilePath -> m Handle
initDb file = do
  fileVar <- liftIO $ newMVar file
  let handle = Handle fileVar
  flip runReaderT handle $ do
    DbUsers.createTables
    DbLists.createTables
    DbTasks.createTables
  return handle

type DbHandler
  = ActionDbCarrier DbUsers.UsersTag
  ( ActionDbCarrier DbLists.ListsTag
  ( ReaderC Context (LiftC Handler)))

handleWithContext :: Context -> DbHandler a -> Handler a
handleWithContext context = runM . runReader context . runActionDb . runActionDb

-- | Lifts a 'Handler' action (like @throwError@) into 'DbHandler'
-- neccessary because I did not implement MTL style classes on it
--
-- lifts:
--   - from 'Handler'  into 'LiftC'
--   - then 'LiftC' into 'ReaderC'
--   - then from 'ReaderC' into the 'ActionDbCarrier' for the Lists-Actions
--   - then into the 'ActionDbCarrier' for the Users-Actions
liftHandler :: Handler a -> DbHandler a
liftHandler = lift . lift . lift . lift
