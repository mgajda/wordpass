-- | Main module generating passwords.
module Main where

import           System.IO       (hFlush, stdout)
import           System.Directory(getDirectoryContents)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Data.Text(Text)
import qualified Data.Set     as Set
import           Data.Char           (isAlpha, isPunctuation, isSymbol)
import           Data.Random.RVar
import           Data.Random.Sample
import           Data.Random.RVar.Enum
import           Data.Random.Distribution
import           Data.Random.Distribution.Uniform
import           Data.Random.Source.IO
import qualified Data.Vector  as V
import           Control.Applicative
import           Control.Monad       (replicateM, foldM)

-- | Reads a dict format to get a list of unique words without any special
--   chars.
readDict ::  FilePath -> IO (V.Vector Text)
readDict filename = do
    input <- Text.readFile filename
    return $! V.fromList . Set.toList . Set.fromList . map stripTails . Text.lines $! input
  where
    stripTails = head . Text.split (not . isAlpha)

-- | Find all plausible dictionaries in a given directory
dictFiles dir = postprocess `fmap`
                  getDirectoryContents dir
  where
    postprocess = map ((dir ++ "/") ++) . filter (not . (=='.') . head)

-- | Default directory where to look for the word lists.
defaultDictDir :: FilePath
defaultDictDir = "/usr/share/dict"

-- | Read a set of dictionaries and put the together.
readDicts filenames = do putStr $ "Reading " ++ show (length filenames) ++ " files"
                         result <- (V.fromList . Set.toList) `fmap` foldM action Set.empty filenames
                         putStrLn ""
                         return result
  where
    action currentSet filename = do newSet <- readDict filename
                                    putStr "."
                                    hFlush stdout
                                    return $! Set.fromList (V.toList newSet) `Set.union` currentSet

-- | Read all dictionaries from a given directory.
readDictDir dirname = dictFiles dirname >>= readDicts

-- | Filename for default dictionary (should be command line argument or default glob.)
defaultDictionary ::  FilePath
defaultDictionary = "/usr/share/dict/british-english"

-- | Take a random element of a vector.
randomElement :: V.Vector a -> RVar a
randomElement words = (words V.!) <$> uniform 0 (V.length words - 1)

-- | Pick a random password, given a words list, and a number of words it will contain.
randomPassword :: V.Vector Text -> Int -> RVar Text
randomPassword words numWords = do ws   <- replicateM numWords $ randomElement words
                                   seps <- replicateM numWords randomSeparator
                                   return $ Text.concat $ zipWith Text.append ws seps

-- | Estimate strength of random password with given inputs.
randomPasswordStrength words numWords = fromIntegral numWords * logBase 2 wordStrength
  where
    wordStrength = fromIntegral $ V.length words * (32 + 100)

-- * Random separators
-- | Randomly pick a word separator as a two-digit number, or a symbol
--   character.
randomSeparator ::  RVar Text
randomSeparator = do b <- uniform False True
                     if b then symbolSeparator
                          else numericSeparator

-- | Two-digit number as a separator 10^2 = 6.6 bits of entropy.
numericSeparator ::  RVar Text
numericSeparator = Text.pack <$> show <$> uniform 0 (99 :: Int)

-- | Conjunction of two unary predicates
(|||) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(|||) f g x = f x || g x

-- | List of symbol and punctuation characters in ASCII
--   Should be 5 bits of entropy
symbolChars ::  V.Vector Char
symbolChars = V.fromList $ filter (isSymbol ||| isPunctuation) $ map toEnum [0..127]

-- | Text with random symbol character, 5 bits of entropy
symbolSeparator ::  RVar Text
symbolSeparator = Text.singleton <$> randomElement symbolChars

main = do dictWords <- readDictDir defaultDictDir
          putStrLn  $ "Read " ++ show (V.length dictWords) ++ " words from dictionaries."
          putStr "Estimated password strength (bits): "
          print $ randomPasswordStrength dictWords numWs
          replicateM 5 $ do 
            rv <- sample $ randomPassword dictWords numWs
            Text.putStrLn rv
  where
    numWs = 5
  
