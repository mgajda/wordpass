{-# LANGUAGE OverlappingInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Data.Random.RVar.Enum() where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Control.Applicative((<$>))

-- TODO: Send to library author as a "missing instance"
instance (Enum a) => Distribution Uniform a where
  rvarT (Uniform l h) = toEnum <$> uniformT (fromEnum l) (fromEnum h)

