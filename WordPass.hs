{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
-- | Main module generating passwords.
module Main where

import qualified Data.Text.IO as Text
import qualified Data.Set     as Set
import qualified Data.Vector  as V
import           Control.Applicative
import           Control.Monad       (replicateM_)
import           HFlags
import           Text.WordPass
import           Test.QuickCheck.Gen(generate)

-- * Command-line flags
-- | Number of words per password
defineFlag "w:words" (4 :: Int) "Number of words for each password."

-- | Number of passwords
defineFlag "p:passwords" (10 :: Int) "Number of passwords to generate."

-- | Default word list directory.
defineFlag "d:directory" ("/usr/share/dict" :: FilePath) "Default directory to search for dictionaries\n (works only if --wordlist options is NOT USED.)"

-- | Pick specific wordlist.
defineFlag "l:wordlist" ("" :: FilePath) "Select particular dictionary (filepath)."

-- | Pick specific wordlist.
defineFlag "t:pseudorandom" (False :: Bool) "Generate passwords using StdRandom generator, instead of DevRandom. (Faster, but less safe. Good for testing."

-- | Fake flag to avoid GHC losing the last instance declaration
defineFlag "fakeflag" (False :: Bool) "This flag does nothing and is never used"


-- | Read wordlist given by explict filepath, or search for all wordlists in a given directory.
selectWordList :: FilePath -> FilePath -> IO WordSet
selectWordList ""       dir = readDictDir dir
selectWordList filename _   = readDict    filename

main :: IO ()
main = do _args <- $initHFlags "WordPass - dictionary-based password generator"
          dictWords <- (V.fromList . Set.toList) <$> selectWordList flags_wordlist flags_directory
          putStrLn  $ "Read " ++ show (V.length dictWords) ++ " words from dictionaries."
          putStr "Estimated password strength (bits): "
          print $ randomPasswordStrength dictWords flags_words
          replicateM_ flags_passwords $ do 
            let rand = randomPassword dictWords flags_words
            rv <- generate rand
            Text.putStrLn rv


