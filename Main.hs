module Main where

import Colourista.Pure
import Data.Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.List.Extra as LE
import Data.Set (Set)
import qualified Data.Set as Set (member)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Options.Applicative
import System.FilePath
import Tachyons (Css, classes, parse)
import Text.Regex.TDFA

-------------------------------------------------------------------------------
-- Options Parsing
-------------------------------------------------------------------------------

newtype Opts = Opts [FilePath]
  deriving (Show)

optsParser :: Parser Opts
optsParser = Opts <$> some (argument str (metavar "FILES..."))

opts :: ParserInfo Opts
opts = info (optsParser <**> helper) idm

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  parsedOpts <- execParser opts
  parsedCss <- parse "tachyons.css"
  either printParseError (run parsedOpts) parsedCss

run :: Opts -> Css -> IO ()
run (Opts files) tachyonsCss = do
  let tachyons = classes tachyonsCss
  results <- mapM (readAndFindMatches tachyons) files
  printResults results

wordsInFile :: Text -> [Text]
wordsInFile fileContent =
  getAllTextMatches (fileContent =~ literalStrRegex)
    >>= Text.splitOn " " . chompQuotes
  where
    literalStrRegex = "\"[^\"]+\"" :: Text
    chompQuotes = Text.init . Text.tail

findMatches :: Set Text -> [Text] -> [Text]
findMatches wanted allWords =
  List.nub $ filter isWanted allWords
  where
    isWanted = flip Set.member wanted

readAndFindMatches :: Set Text -> FilePath -> IO (Text, [Text])
readAndFindMatches tachyons file = do
  let fileName = Text.pack $ takeFileName file
  fileContent <- TextIO.readFile file
  let matches = findMatches tachyons $ wordsInFile fileContent
  pure (fileName, matches)

-------------------------------------------------------------------------------
-- Display
-------------------------------------------------------------------------------

printParseError :: String -> IO ()
printParseError err = do
  print ("Failed parsing tachyons.css" :: String)
  print err

printResults :: [(Text, [Text])] -> IO ()
printResults = mapM_ printFileResult
  where
    printFileResult :: (Text, [Text]) -> IO ()
    printFileResult (_, []) = pure ()
    printFileResult (fileName, results) = do
      TextIO.putStrLn $ formatFileName fileName
      TextIO.putStrLn $ formatResults results
      putStrLn ""

    formatFileName :: Text -> Text
    formatFileName name = formatWith [magenta] name

    formatResults results =
      results
        <&> Text.justifyLeft 20 ' '
        & List.intercalate ["\n"] . LE.chunksOf 4
        & fold
