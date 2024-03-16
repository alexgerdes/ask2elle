{-# OPTIONS_GHC -Wno-orphans #-}

module GhcLib.GHCRelated.Bag() where

import GHC.Data.Bag qualified as GHC

instance Show a => Show (GHC.Bag a) where
    show :: Show a => GHC.Bag a -> String
    show = show . GHC.bagToList