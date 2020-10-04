module FindReplace (StringMatch, findStrings, matchText) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Regex.TDFA

data StringMatch = StringMatch
  { matchText :: Text,
    matchOffset :: Int,
    matchLength :: Int
  }
  deriving (Show)

findStrings :: Text -> [StringMatch]
findStrings fileContent =
  let indexMatches = getAllMatches (fileContent =~ literalStrRegex) :: [(Int, Int)]
      textMatches = getAllTextMatches (fileContent =~ literalStrRegex) :: [Text]
   in zipWith mkMatch indexMatches textMatches
  where
    literalStrRegex = "\"[^\"]+\"" :: Text
    chompQuotes = Text.init . Text.tail
    mkMatch index text =
      StringMatch
        { matchText = chompQuotes text,
          matchOffset = fst index + 1, -- Don't include first quote
          matchLength = snd index - 2 -- Subtract surrounding quotes
        }
