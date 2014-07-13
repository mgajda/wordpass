{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
-- | Main module generating passwords.
module Main where

import           Data.Ratio
import           System.IO       (hFlush, stdout)
import           System.Directory
import           System.FilePath ((</>), takeDirectory)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Data.Text(Text)
import qualified Data.Set     as Set
import           Data.Set (Set)
import           Data.Char           (isAlpha, isPunctuation, isSymbol)
import           Data.Random.RVar
import           Data.Random.Sample
import           Data.Random.RVar.Enum
import           Data.Random.Choice
import           Data.Random.Vector
import           Data.Random.Source.DevRandom
import           Data.Random.Distribution
import           Data.Random.Distribution.Uniform
import           Data.Random.Source.IO
import qualified Data.Vector  as V
import           Control.Applicative
import           Control.Monad       (replicateM, foldM, filterM)
import           Control.DeepSeq
import           HFlags
import           System.PosixCompat

type WordList = Set.Set Text

-- | Try to resolve symbolic link chain for given filename.
resolveSymbolicLink s = do b <- isSymbolicLink `fmap` getSymbolicLinkStatus s
                           if b
                             then do newPath <- readSymbolicLink s
                                     resolveSymbolicLink $! takeDirectory s </> newPath 
                             else return s

-- | Reads a dict format to get a list of unique words without any special
--   chars.
readDict ::  FilePath -> IO WordList
readDict filename = do
    input <- Text.readFile filename
    return $! Set.fromList . map stripTails . Text.lines $! input
  where
    stripTails = head . Text.split (not . isAlpha)

-- | Find all plausible dictionaries in a given directory
dictFiles dir = do candidates <- preprocess `fmap` prefilter `fmap`
                                   getDirectoryContents dir
                   resolvedCandidates <- uniquify `fmap` mapM resolveSymbolicLink candidates
                   result <- filterM checkPerms resolvedCandidates
                   print result
                   return result
  where
    preprocess = map ((dir ++ "/") ++)
    prefilter  = filter (not . (`elem` ".~_") . head) . filter (not . ("README" `isPrefixOf`))
    checkPerms filename = do perms <- getPermissions filename
                             return $!      readable   perms  &&
                                       not (executable perms) &&
                                       not (searchable perms)
    uniquify = Set.toList . Set.fromList
    isPrefixOf ""     a               = True
    isPrefixOf bs     ""              = False
    isPrefixOf (b:bs) (c:cs) | b == c = isPrefixOf bs cs
    isPrefixOf _      _               = False

-- | Read a set of dictionaries and put the together.
readDicts filenames = do putStr $ "Reading " ++ show (length filenames) ++ " files"
                         result <- foldM action Set.empty filenames
                         putStrLn ""
                         return result
  where
    action currentSet filename = do newSet <- readDict filename
                                    let result = newSet `Set.union` currentSet
                                    putStr "."
                                    hFlush stdout
                                    result `deepseq` return result

-- | Read all dictionaries from a given directory.
readDictDir dirname = dictFiles dirname >>= readDicts

-- | Filename for default dictionary (should be command line argument or default glob.)
defaultDictionary ::  FilePath
defaultDictionary = "/usr/share/dict/british-english"

-- | Pick a random password, given a words list, and a number of words it will contain.
randomPassword :: V.Vector Text -> Int -> RVar Text
randomPassword words numWords = do ws   <- replicateM numWords $ randomElement words
                                   seps <- replicateM numWords randomSeparator
                                   return $ Text.concat $ zipWith Text.append ws seps

-- | Estimate strength of random password with given inputs.
randomPasswordStrength words numWords = fromIntegral numWords * logBase 2 wordStrength
  where
    wordStrength = fromIntegral $ V.length words * (numSymbols + numNumericSeparators)

numSymbols  = V.length symbolChars -- 32
numNumericSeparators = 100

-- * Random separators
-- | Randomly pick a word separator as a two-digit number, or a symbol
--   character.
randomSeparator ::  RVar Text
randomSeparator = randomChoice ratio  randomSymbolSeparator randomNumericSeparator
  where
    ratio = numSymbols % numNumericSeparators

-- | Two-digit number as a separator 10^2 = 6.6 bits of entropy.
randomNumericSeparator ::  RVar Text
randomNumericSeparator = Text.pack <$> show <$> uniform 0 (numNumericSeparators :: Int)

-- | Conjunction of two unary predicates
(|||) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(|||) f g x = f x || g x

-- | List of symbol and punctuation characters in ASCII
--   Should be 5 bits of entropy
symbolChars ::  V.Vector Char
symbolChars = V.fromList $ filter (isSymbol ||| isPunctuation) $ map toEnum [0..127]

-- | Text with random symbol character, 5 bits of entropy
randomSymbolSeparator ::  RVar Text
randomSymbolSeparator = Text.singleton <$> randomElement symbolChars

-- * Command-line flags
-- | Number of words per password
defineFlag "w:words" (4 :: Int) "Number of words for each password."

-- | Number of passwords
defineFlag "p:passwords" (10 :: Int) "Number of passwords to generate."

-- | Default word list directory.
defineFlag "d:directory" ("/usr/share/dict" :: FilePath) ("Default directory to search for dictionaries\n (works only if --wordlist options is NOT USED.)")

-- | Pick specific wordlist.
defineFlag "l:wordlist" ("" :: FilePath) "Select particular dictionary (filepath)."

-- | Pick specific wordlist.
defineFlag "t:pseudorandom" (False :: Bool) "Generate passwords using StdRandom generator, instead of DevRandom. (Faster, but less safe. Good for testing."

-- | Fake flag to avoid GHC losing the last instance declaration
defineFlag "fakeflag" (False :: Bool) "This flag does nothing and is never used"


-- | Read wordlist given by explict filepath, or search for all wordlists in a given directory.
selectWordList :: FilePath -> FilePath -> IO WordList
selectWordList ""       dir = readDictDir dir
selectWordList filename _   = readDict    filename

main = do $initHFlags "WordPass - dictionary-based password generator"
          dictWords <- (V.fromList . Set.toList) `fmap` selectWordList flags_wordlist flags_directory
          putStrLn  $ "Read " ++ show (V.length dictWords) ++ " words from dictionaries."
          putStr "Estimated password strength (bits): "
          print $ randomPasswordStrength dictWords flags_words
          replicateM flags_passwords $ do 
            let rand = randomPassword dictWords flags_words
            rv <- if flags_pseudorandom
                    then sample rand
                    else sampleFrom DevRandom rand
            Text.putStrLn rv


