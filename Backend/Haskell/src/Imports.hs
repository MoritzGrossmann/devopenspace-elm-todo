{-# LANGUAGE ConstraintKinds #-}
module Imports
  ( Has 
  ) where

import Control.Effect.Carrier (Carrier, Member)

type Has eff sig m = (Carrier sig m, Member eff sig)