module Main where

import qualified Data.Functor.Identity(Identity)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Data.Text(Text)
import qualified Data.Set     as Set
import           Data.Char           (isAlpha, isPunctuation, isSymbol)
import           Data.Random.RVar
import           Data.Random.RVar.Enum
import           Data.Random.Distribution
import           Data.Random.Distribution.Uniform
import           Data.Random.Source.DevRandom
import qualified Data.Vector  as V
import           Control.Applicative
import           Control.Monad       (replicateM)

readDict ::  FilePath -> IO (V.Vector Text)
readDict filename = do
    input <- Text.readFile filename
    return $! V.fromList . Set.toList . Set.fromList . map stripTails . Text.lines $! input
  where
    stripTails = head . Text.split (not . isAlpha)

defaultDictionary ::  FilePath
defaultDictionary = "/usr/share/dict/british-english"

randomElement :: V.Vector a -> RVar a
randomElement words = (words V.!) <$> uniform 0 (V.length words - 1)

randomPassword :: V.Vector Text -> Int -> RVar Text
randomPassword words numWords = do ws   <- replicateM numWords $ randomElement words
                                   seps <- replicateM numWords randomSeparator
                                   return $ Text.concat $ zipWith Text.append ws seps

randomSeparator ::  RVar Text
randomSeparator = do b <- uniform False True
                     if b then symbolSeparator
                          else numericSeparator

numericSeparator ::  RVar Text
numericSeparator = Text.pack <$> show <$> uniform 0 (99 :: Int)

(|||) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(|||) f g x = f x || g x

symbolChars ::  V.Vector Char
symbolChars = V.fromList $ filter (isSymbol ||| isPunctuation) $ map toEnum [0..127]

symbolSeparator ::  RVarT Data.Functor.Identity.Identity Text
symbolSeparator = Text.singleton <$> randomElement symbolChars

sampleRV = flip runRVar DevRandom

main = do dictWords <- readDict defaultDictionary
          print $ V.length dictWords
          replicateM 10 $ do 
            rv <- sampleRV $ randomPassword dictWords 3
            Text.putStrLn rv
  
