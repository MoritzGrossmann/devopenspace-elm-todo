
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Db.Carrier
  ( DbHandler
  , ActionDbCarrier (..)
  , handleWithDb
  , runActionDb
  ) where

import           Control.Effect.Lift (LiftC, runM)
import           Control.Effect.Reader (ReaderC)
import qualified Control.Effect.Reader as R
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Db.Internal
import           Servant (Handler)


type DbHandler = ActionDbCarrier (ReaderC Handle (LiftC Handler))

handleWithDb :: Handle -> DbHandler a -> Handler a
handleWithDb dbHandle = runM . R.runReader dbHandle . runActionDb

newtype ActionDbCarrier m a
  = ActionDbCarrier { runActionDbCarrier :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans ActionDbCarrier where
  lift m = ActionDbCarrier m

runActionDb :: ActionDbCarrier m a -> m a
runActionDb = runActionDbCarrier
