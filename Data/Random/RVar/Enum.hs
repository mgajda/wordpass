{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
-- | Provides a random variable (@RVar@) instance for any @Enum@ instance.
-- Very convenient, even if may overlap with more specific instances for integer types.
module Data.Random.RVar.Enum() where

import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Control.Applicative((<$>))

-- TODO: Send to library author as a "missing instance"
instance (Enum a) => Distribution Uniform a where
  rvarT (Uniform l h) = toEnum <$> uniformT (fromEnum l) (fromEnum h)

