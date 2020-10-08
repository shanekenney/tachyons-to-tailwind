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
import SourceFile (readSourceFile, tachyonsInFile)
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

list :: Set Text -> [FilePath] -> IO ()
list tachyons files = do
  results <- traverse (readAndFindMatches tachyons) files
  printResults results

replace :: Set Text -> [FilePath] -> ReplaceMode -> IO ()
replace _tachyons _files mode = do
  putStrLn "Not implemented."
  print mode

run :: Cmd -> Css -> IO ()
run cmd tachyonsCss =
  let tachyons = classes tachyonsCss
   in case cmd of
        (List files) -> list tachyons files
        (Replace files mode) -> replace tachyons files mode

findMatches :: Set Text -> [Text] -> [Text]
findMatches wanted allWords =
  List.nub $ filter isWanted allWords
  where
    isWanted = flip Set.member wanted

readAndFindMatches :: Set Text -> FilePath -> IO (FilePath, [Text])
readAndFindMatches tachyons file = do
  fileContent <- TextIO.readFile file
  let sourceFile = readSourceFile tachyons fileContent
  let matches = List.nub $ tachyonsInFile sourceFile
  pure (file, matches)

testContent :: Text
testContent = "div [ class \"flex fixed z-999 bg-light-yellow bottom-0 left-0\""

-------------------------------------------------------------------------------
-- Display
-------------------------------------------------------------------------------

printParseError :: String -> IO ()
printParseError err = do
  putStrLn ("Failed parsing tachyons.css" :: String)
  putStrLn err

printResults :: [(FilePath, [Text])] -> IO ()
printResults = traverse_ printFileResult
  where
    printFileResult :: (FilePath, [Text]) -> IO ()
    printFileResult (_, []) = pure ()
    printFileResult (fileName, results) = do
      putStrLn $ formatWith [magenta] fileName
      TextIO.putStrLn $ formatResults results
      putStrLn ""

    formatResults :: [Text] -> Text
    formatResults results =
      results
        <&> Text.justifyLeft 20 ' '
          & List.intercalate ["\n"] . LE.chunksOf 4
          & fold
