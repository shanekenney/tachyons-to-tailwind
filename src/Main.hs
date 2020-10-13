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
import SourceFile
import Tachyons (Css, classes, parse)

-------------------------------------------------------------------------------
-- Options Parsing
-------------------------------------------------------------------------------

data ReplaceMode
  = Normal
  | DryRun
  deriving (Show)

data Cmd
  = List [FilePath]
  | Replace [FilePath] ReplaceMode
  deriving (Show)

cmdParser :: Parser Cmd
cmdParser =
  hsubparser $
    mconcat
      [ command "list" (info listParse (progDesc "Find and list all tachyons classes used")),
        command "replace" (info replaceParse (progDesc "Replace tachyons classes with matching tailwind classes"))
      ]
  where
    fileArgs :: Parser [FilePath]
    fileArgs = some (argument str (metavar "FILES..."))

    listParse :: Parser Cmd
    listParse = List <$> fileArgs

    replaceParse :: Parser Cmd
    replaceParse =
      Replace <$> fileArgs <*> flag Normal DryRun (long "dry-run")

opts :: ParserInfo Cmd
opts = info (cmdParser <**> helper) idm

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  parsedOpts <- execParser opts
  parsedCss <- parse "tachyons.css"
  either printParseError (run parsedOpts) parsedCss

run :: Cmd -> Css -> IO ()
run cmd tachyonsCss =
  let tachyons = classes tachyonsCss
   in case cmd of
        (List files) -> list tachyons files
        (Replace files mode) -> replace tachyons files mode

list :: Set Text -> [FilePath] -> IO ()
list tachyons files = do
  results <- traverse readAndFindMatches files
  printListResults results
  where
    readAndFindMatches :: FilePath -> IO (FilePath, [Text])
    readAndFindMatches file = do
      fileContent <- TextIO.readFile file
      let sourceFile = readSourceFile (`Set.member` tachyons) fileContent
      let matches = List.nub $ tachyonsInFile sourceFile
      pure (file, matches)

dropThird :: (a, b, c) -> (a, b)
dropThird (a, b, _) = (a, b)

third :: (a, b, c) -> c
third (_, _, c) = c

isDryRun :: ReplaceMode -> Bool
isDryRun DryRun = True
isDryRun Normal = False

replace :: Set Text -> [FilePath] -> ReplaceMode -> IO ()
replace tachyons files mode = do
  results <- traverse readAndReplaceMatches files
  let replacements = dropThird <$> results
  let noReplacements = concat $ third <$> results
  printReplaceResults (isDryRun mode) replacements
  printNoReplacements $ List.nub noReplacements
  where
    readAndReplaceMatches :: FilePath -> IO (FilePath, [(Text, Text)], [Text])
    readAndReplaceMatches file = do
      fileContent <- TextIO.readFile file
      let sourceFile = readSourceFile (`Set.member` tachyons) fileContent
      let replacements = replacementsInFile sourceFile
      let noReplacements = noReplacementsInFile sourceFile
      if not (isDryRun mode)
        then TextIO.writeFile file $ writeSourceFile sourceFile
        else pure ()
      pure (file, replacements, noReplacements)

-------------------------------------------------------------------------------
-- Display
-------------------------------------------------------------------------------

printParseError :: String -> IO ()
printParseError err = do
  putStrLn ("Failed parsing tachyons.css" :: String)
  putStrLn err

printListResults :: [(FilePath, [Text])] -> IO ()
printListResults = traverse_ printListResult
  where
    printListResult :: (FilePath, [Text]) -> IO ()
    printListResult (_, []) = pure ()
    printListResult (fileName, results) = do
      putStrLn $ formatWith [magenta] fileName
      TextIO.putStrLn $ formatResults results
      putStrLn ""

    formatResults :: [Text] -> Text
    formatResults results =
      results
        <&> Text.justifyLeft 20 ' '
          & List.intercalate ["\n"] . LE.chunksOf 4
          & fold

printReplaceResults :: Bool -> [(FilePath, [(Text, Text)])] -> IO ()
printReplaceResults dryRun =
  traverse_ printReplaceResult
  where
    printReplaceResult :: (FilePath, [(Text, Text)]) -> IO ()
    printReplaceResult (fileName, []) = do
      putStr $
        if dryRun
          then formatWith [bold, magenta] "[Dry run] "
          else ""
      putStrLn $ formatWith [magenta] fileName
      putStrLn "No matching replacements"
      putStrLn ""
    printReplaceResult (fileName, results) = do
      putStr $
        if dryRun
          then formatWith [bold, magenta] "[Dry run] "
          else ""
      putStrLn $ formatWith [magenta] fileName
      TextIO.putStrLn $ formatResults results
      putStrLn ""

    formatResults :: [(Text, Text)] -> Text
    formatResults results =
      let formatResult (from, to) = Text.concat [Text.justifyLeft 16 ' ' from, " -> ", to]
       in results
            <&> formatResult
            <&> Text.justifyLeft 40 ' '
              & List.intercalate ["\n"] . LE.chunksOf 2
              & fold

printNoReplacements :: [Text] -> IO ()
printNoReplacements [] = pure ()
printNoReplacements noReplacements = do
  putStrLn $ formatWith [bold, blue] "No replacement rules were found for the following classes:"
  TextIO.putStrLn $ formatResults noReplacements
  putStrLn ""
  where
    formatResults :: [Text] -> Text
    formatResults results =
      results
        <&> Text.justifyLeft 20 ' '
          & List.intercalate ["\n"] . LE.chunksOf 4
          & fold
