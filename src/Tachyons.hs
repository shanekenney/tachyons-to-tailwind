module Tachyons (Css, classes, parse) where

import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Data.Text as Text (Text, tail)
import Data.Text.IO as TextIO (readFile)
import Paths_tachyons_to_tailwind (getDataFileName)
import Text.CSS.Parse (NestedBlock (..), parseNestedBlocks)
import Text.Regex.TDFA

newtype Css = Css [NestedBlock]

parse :: String -> IO (Either String Css)
parse dataFile = do
  file <- getDataFileName dataFile
  content <- TextIO.readFile file
  pure $ Css <$> parseNestedBlocks content

selectorsInBlock :: NestedBlock -> [Text]
selectorsInBlock (NestedBlock _ css) = selectors css
selectorsInBlock (LeafBlock cssBlock) = [selector]
  where
    selector = fst cssBlock

selectors :: [NestedBlock] -> [Text]
selectors = foldr getSelectors []
  where
    getSelectors block selectors' =
      selectorsInBlock block ++ selectors'

classes :: Css -> Set Text
classes (Css css) = Set.fromList (selectors css >>= getClasses)
  where
    className = Text.tail

    classRegex :: Text
    classRegex = "\\.[a-z0-9\\-]+"

    getClasses :: Text -> [Text]
    getClasses selector = className <$> getAllTextMatches (selector =~ classRegex)
