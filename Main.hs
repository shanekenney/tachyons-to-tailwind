module Main where

import Control.Monad
import Data.Function ((&))
import Data.List (intercalate)
import Data.Text as Text (Text, unpack)
import qualified Data.Text.IO as Text.IO
import Options.Applicative
import Tachyons (NestedBlock, classes, parse)

-------------------------------------------------------------------------------
-- Options Parsing
-------------------------------------------------------------------------------

data Opts = Opts [FilePath]
  deriving (Show)

optsParser :: Parser Opts
optsParser = Opts <$> some (argument str (metavar "FILES..."))

opts :: ParserInfo Opts
opts = info (optsParser <**> helper) idm

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = execParser opts >>= run

run :: Opts -> IO ()
run (Opts files) = do
  let dataFile = "tachyons.css"
  readAndDisplayFiles files
  result <- parse dataFile
  either (printParseError dataFile) processTachyons result

processTachyons :: [NestedBlock] -> IO ()
processTachyons = printClasses . classes

-------------------------------------------------------------------------------
-- Display
-------------------------------------------------------------------------------

printParseError :: String -> String -> IO ()
printParseError dataFile err = do
  print $ "Failed parsing " ++ dataFile
  print err

printClasses :: [Text] -> IO ()
printClasses classes' =
  classes'
    & map unpack
    & intercalate "\n"
    & putStrLn

readAndDisplayFiles :: [FilePath] -> IO ()
readAndDisplayFiles = mapM_ $ Text.IO.readFile >=> Text.IO.putStrLn
