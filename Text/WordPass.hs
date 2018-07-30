{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- | Main module generating passwords.
module Text.WordPass where

import           Data.Monoid((<>))
import           System.IO       (hFlush, stdout)
import           System.Directory hiding (isSymbolicLink)
import           System.FilePath ((</>), takeDirectory)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Data.Text(Text)
import qualified Data.Set     as Set
import           Data.Set (Set)
import           Data.Char           (isAlpha, isPunctuation, isSymbol, toLower, toUpper)
import           Test.QuickCheck.Gen
import qualified Data.Vector  as V
import           Control.Applicative
import           Control.Monad       (replicateM, foldM, filterM)
import           Control.DeepSeq
import           System.PosixCompat  (isSymbolicLink, readSymbolicLink, getSymbolicLinkStatus)

-- | Explanatory type alias for the type of wordlists during preprocessing.
type WordSet  = Set.Set Text

-- | Explanatory type alias for immutable, preprocessed wordlist used by random number generator.
type WordList = V.Vector Text

-- * Reading inputs
-- | Try to resolve symbolic link chain for given filename.
resolveSymbolicLink ::  FilePath -> IO FilePath
resolveSymbolicLink s = do b <- isSymbolicLink `fmap` getSymbolicLinkStatus s
                           if b
                             then do newPath <- readSymbolicLink s
                                     resolveSymbolicLink $! takeDirectory s </> newPath
                             else return s

-- | Reads a dict format to get a list of unique words without any special
--   chars.
readDict ::  FilePath -> IO WordSet
readDict filename = do
    input <- Text.readFile filename
    return $! Set.fromList . map stripTails . Text.lines $! input
  where
    stripTails = head . Text.split (not . isAlpha)

-- | Find all plausible dictionaries in a given directory
dictFiles ::  FilePath -> IO [FilePath]
dictFiles dir = do candidates <- preprocess `fmap` prefilter `fmap`
                                   getDirectoryContents dir
                   resolvedCandidates <- nubSet `fmap` mapM resolveSymbolicLink candidates
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
    nubSet = Set.toList . Set.fromList
    isPrefixOf :: String -> String -> Bool
    isPrefixOf ""     _               = True
    isPrefixOf _      ""              = False
    isPrefixOf (b:bs) (c:cs) | b == c = bs `isPrefixOf` cs
    isPrefixOf _      _               = False

-- | Read a set of dictionaries and put the together.
readDicts ::  [FilePath] -> IO (Set Text)
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
readDictDir ::  FilePath -> IO (Set Text)
readDictDir dirname = dictFiles dirname >>= readDicts

-- | Filename for default dictionary (should be command line argument or default glob.)
defaultDictionary ::  FilePath
defaultDictionary = "/usr/share/dict/british-english"

-- | Pick a random password, given a words list, and a number of words it will contain.
randomPassword :: WordList -> Int -> Gen Text
randomPassword wordlist numWords = do ws    <- replicateM numWords $ randomCase $ randomElement wordlist
                                      seps  <- replicateM numWords   randomSeparator
                                      return $ Text.concat $ zipWith Text.append ws seps

-- | First character uppercase, all others lowercase
capitalized :: Text -> Text
capitalized word = Text.toUpper first `Text.append` Text.toLower rest
  where
    (first, rest) = Text.splitAt 1 word

-- | First character lowercase, all others uppercase
uncapitalized :: Text -> Text
uncapitalized word = Text.toLower first `Text.append` Text.toUpper rest
  where
    (first, rest) = Text.splitAt 1 word

-- | Swap case for each letter, starting from upper
evenUpperOddLower :: Text -> Text
evenUpperOddLower = Text.pack . go . Text.unpack
  where
    go :: String -> String
    go []       = []
    go [a]      = [toLower a]
    go (a:b:cs) =  toLower a:toUpper b:go cs

-- | Swap case, starting from lower
evenLowerOddUpper :: Text -> Text
evenLowerOddUpper = Text.pack . go . Text.unpack
  where
    go :: String -> String
    go []       = []
    go [a]      = [toUpper a]
    go (a:b:cs) =  toUpper a:toLower b:go cs

-- | Randomize letter case within the word.
randomCase :: Gen Text -> Gen Text
randomCase wordGen = do
  word    <- wordGen
  changer <- elements caseVariants
  return $ changer word

-- | Different uppercase/lowercase variants of each word.
caseVariants :: [Text -> Text]
caseVariants = [capitalized,       uncapitalized,
                Text.toLower,      Text.toUpper,
                evenLowerOddUpper, evenUpperOddLower]

-- | Estimate strength of random password with given inputs.
randomPasswordStrength :: V.Vector a -> Int -> Double
randomPasswordStrength wordlist numWords = fromIntegral numWords * (logBase 2 wordStrength)
  where
    wordStrength = fromIntegral $ V.length wordlist * (numSymbols + numNumericSeparators) * length caseVariants

-- | Number of characters within alphabet.
numSymbols ::  Int
numSymbols  = V.length symbolChars -- 32

-- | Since we use two-digit separators, there are 100 different.
numNumericSeparators ::  Int
numNumericSeparators = 100

-- * Random separators
-- | Randomly pick a word separator as a two-digit number, or a symbol
--   character.
randomSeparator :: Gen Text
randomSeparator = do
    r <- choose (0.0, 1.0::Double)
    if r > ratio
       then randomSymbolSeparator
       else randomNumericSeparator
  where
    ratio :: Double = fromIntegral numSymbols / fromIntegral(numNumericSeparators+numSymbols)

-- | Two-digit number as a separator 10^2 = 6.6 bits of entropy.
randomNumericSeparator ::  Gen Text
randomNumericSeparator = Text.pack . show <$> choose (0, numNumericSeparators-1)

-- | Conjunction of two unary predicates
(|||) ::  (t -> Bool) -> (t -> Bool) -> t -> Bool
(|||) f g x = f x || g x

randomElement  :: V.Vector a -> Gen a
randomElement v = (v V.!) <$> choose (0, V.length v-1)

-- | List of symbol and punctuation characters in ASCII
--   Should be 5 bits of entropy
symbolChars ::  V.Vector Char
symbolChars = V.fromList $ filter (isSymbol ||| isPunctuation) $ map toEnum [0..127]

-- | Text with random symbol character, 5 bits of entropy
randomSymbolSeparator ::  Gen Text
randomSymbolSeparator = Text.singleton <$> randomElement symbolChars
