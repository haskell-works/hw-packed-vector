{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( CreateIndexOptions(..)
  ) where

import GHC.Generics
import HaskellWorks.Data.Positioning

data CreateIndexOptions = CreateIndexOptions
  { wordSize :: Count
  , input    :: FilePath
  , output   :: FilePath
  } deriving (Eq, Show, Generic)
