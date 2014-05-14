module Main where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Data.Text(Text)
import qualified Data.Set     as Set
import           Data.Char           (isAlpha)
import           Data.Random.RVar
import           Data.Random.Distribution.Uniform
import           Data.Random.Source.DevRandom
import qualified Data.Vector  as V
import           Control.Applicative

readDict filename = do
    input <- Text.readFile filename
    return $! V.fromList . Set.toList . Set.fromList . map stripTails . Text.lines $! input
  where
    stripTails = head . Text.split (not . isAlpha)

defaultDictionary = "/usr/share/dict/british-english"

newtype Password = Password Text
  deriving (Show, Eq, Ord)

randomWord words = (words V.!) <$> uniform 0 (V.length words)

randomPassword words numWords = undefined

sampleRV = flip runRVar DevRandom

main = do dictWords <- readDict defaultDictionary
          print $ V.length dictWords
          rv <- sampleRV $ randomWord dictWords
          print rv
  
