{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
-- | Main module generating passwords.
module Main where

import qualified Data.Text.IO as Text
import qualified Data.Set     as Set
import qualified Data.Vector  as V
import           Control.Applicative
import           Control.Monad       (replicateM_)
import           Options.Applicative
import           Text.WordPass
import           Test.QuickCheck.Gen(generate)

-- * Command-line flags
data Options = Options {
    optWords :: Int
    -- ^ Number of words per password
  , optCount :: Int
  -- ^ Number of passwords
  , optWordlist :: Either FilePath FilePath
  -- ^ Default word list directory or pick specific wordlist.
  } deriving (Show)

options :: Parser Options
options  = Options
        <$> option auto (short 'w' <>
                       long "words" <>
                       metavar "INT" <>
                       value 4 <>
                       help "Number of words for each password.")
        <*> option auto (short 'p' <>
                       long "passwords" <>
                       metavar "COUNT" <>
                       value 10 <>
                       help "Number of passwords to generate.")
        <*> ((Left <$> strOption (short 'd' <>
                                 long "directory" <>
                                 metavar "DIR" <>
                                 value "/usr/share/dict" <>
                                 help "Default directory to search for dictionaries."))
         <|> (Right <$> strOption (short 'd' <>
                                   long "directory" <>
                                   metavar "DIR" <>
                                   value "/usr/share/dict" <>
                                   help "Default directory to search for dictionaries.")))

-- | Read wordlist given by explict filepath, or search for all wordlists in a given directory.
selectWordList :: Either FilePath FilePath -> IO WordSet
selectWordList (Left  dir     ) = readDictDir dir
selectWordList (Right filename) = readDict    filename

parseOptions = info (options <**> helper)
                    (fullDesc <>
                     progDesc "Generate xkcd-style passwords" <>
                     header "WordPass - dictionary-based password generator")

main :: IO ()
main = do Options {..} <- execParser parseOptions
          dictWords <- (V.fromList . Set.toList) <$> selectWordList optWordlist
          putStrLn  $ "Read " ++ show (V.length dictWords) ++ " words from dictionaries."
          putStr "Estimated password strength (bits): "
          print $ randomPasswordStrength dictWords optWords
          replicateM_ optCount $ do
            let rand = randomPassword dictWords optWords
            rv <- generate rand
            Text.putStrLn rv
