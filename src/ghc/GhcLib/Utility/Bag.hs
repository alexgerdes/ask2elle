{-# OPTIONS_GHC -Wno-orphans #-}
module GhcLib.Utility.Bag where

import qualified GHC.Data.Bag as GHC


instance Show a => Show (GHC.Bag a) where 
    show :: GHC.Bag a -> String
    show = show . GHC.bagToList 