module Main where

import Data.Function ((&))
import Data.List (intercalate)
import Data.Text as Text (Text, unpack)
import Tachyons (NestedBlock, classes, parse)

main :: IO ()
main = do
  let dataFile = "tachyons.css"
  result <- parse dataFile
  either (printParseError dataFile) processTachyons result

printParseError :: String -> String -> IO ()
printParseError dataFile err = do
  print $ "Failed parsing " ++ dataFile
  print err

processTachyons :: [NestedBlock] -> IO ()
processTachyons = printClasses . classes

printClasses :: [Text] -> IO ()
printClasses classes' =
  classes'
    & map unpack
    & intercalate "\n"
    & putStrLn
