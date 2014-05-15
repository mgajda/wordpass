{-# LANGUAGE FlexibleContexts #-}
-- | Main module generating passwords.
module Data.Random.Choice(randomChoice) where

import           Data.Ratio
import           Data.Random.RVar
import           Data.Random.Distribution
import           Data.Random.Distribution.Uniform

-- | Performs random choice between two RVar values.
--   Input is a _ratio_ of the _relative_ probabilities between first and
--   second option (A/B).
randomChoice :: (Fractional r, Ord r, Data.Random.Distribution.Distribution Uniform r) => r ->
                   RVar b -> RVar b -> RVar b
randomChoice ratio variantA variantB = do draw <- uniform 0 1
                                          if draw >= probabilityOfVariantA
                                            then variantA
                                            else variantB
  where
    probabilityOfVariantA = 1/(1+1/ratio)


