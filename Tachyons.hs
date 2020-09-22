module Tachyons (classes, parse, selectors, NestedBlock) where

import Data.Text as Text (Text, tail)
import Data.Text.IO as TextIO (readFile)
import Paths_tachyons_to_tailwind (getDataFileName)
import Text.CSS.Parse (NestedBlock (..), parseNestedBlocks)
import Text.Regex.TDFA

parse :: String -> IO (Either String [NestedBlock])
parse dataFile =
  getDataFileName dataFile
    >>= TextIO.readFile
    >>= return . parseNestedBlocks

selectorsInBlock :: NestedBlock -> [Text]
selectorsInBlock (NestedBlock _ css) = selectors css
selectorsInBlock (LeafBlock cssBlock) = [selector]
  where
    selector = fst cssBlock

selectors :: [NestedBlock] -> [Text]
selectors css = foldr getSelectors [] css
  where
    getSelectors block selectors' =
      selectorsInBlock block ++ selectors'

classes :: [NestedBlock] -> [Text]
classes css = selectors css >>= getClasses
  where
    className = Text.tail

    classRegex :: Text
    classRegex = "\\.[a-z0-9\\-]+"

    getClasses :: Text -> [Text]
    getClasses selector = className <$> getAllTextMatches (selector =~ classRegex)
