{-# LANGUAGE ConstraintKinds #-}
module Imports
  ( Has
  , Carrier(..), Member
  , HFunctor
  , send
  , Effect
  , (:+:)(..)
  , handleCoercible
  , MonadIO
  , liftIO
  , module Servant
  ) where

import Control.Effect (Effect)
import Control.Effect.Carrier (HFunctor, send, Member, Carrier(..), (:+:)(..), handleCoercible)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Servant hiding (Context)

type Has eff sig m = (Carrier sig m, Member eff sig)
