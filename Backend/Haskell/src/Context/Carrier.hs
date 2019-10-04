{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Context.Carrier
  ( ContextHandler
  , ContextActionCarrier (..)
  , handleWithinContext
  , runContextAction
  ) where

import           Context.Internal
import           Control.Effect.Lift (LiftC, runM)
import           Control.Effect.Reader (ReaderC)
import qualified Control.Effect.Reader as R
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Servant (Handler)

type ContextHandler = ContextActionCarrier (ReaderC Context (LiftC Handler))

handleWithinContext :: Context -> ContextHandler a -> Handler a
handleWithinContext ctx = runM . R.runReader ctx . runContextAction

newtype ContextActionCarrier m a
  = ContextActionCarrier { runContextActionCarrier :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans ContextActionCarrier where
  lift m = ContextActionCarrier m

runContextAction :: ContextActionCarrier m a -> m a
runContextAction = runContextActionCarrier
