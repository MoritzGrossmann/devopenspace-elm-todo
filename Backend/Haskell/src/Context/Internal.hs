{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Context.Internal
  ( Context (..)
  ) where

import qualified Db.Internal as Db

data Context = Context
  { contextDbHandle :: Db.Handle
  }
