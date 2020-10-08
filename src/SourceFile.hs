module SourceFile (SourceFile, readSourceFile, tachyonsInFile) where

import Data.Set (Set)
import qualified Data.Set as Set (member)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Regex.TDFA

data SourceFile = SourceFile
  { sourceFileText :: Text,
    sourceFileStrings :: [StringLiteral]
  }
  deriving (Show)

data StringLiteral = StringLiteral
  { stringIndex :: Int,
    stringLength :: Int,
    stringWords :: [WordMatch]
  }
  deriving (Show)

data WordMatch
  = Other
  | Replacement Text Text
  | NoReplacement Text
  deriving (Show, Eq)

readSourceFile :: Set Text -> Text -> SourceFile
readSourceFile tachyons fileContent =
  SourceFile
    { sourceFileText = fileContent,
      sourceFileStrings = stringLiterals
    }
  where
    stringLiterals :: [StringLiteral]
    stringLiterals =
      let indexMatches = getAllMatches (fileContent =~ literalStrRegex) :: [(Int, Int)]
          textMatches = getAllTextMatches (fileContent =~ literalStrRegex) :: [Text]
       in zipWith mkLiteral indexMatches textMatches
      where
        literalStrRegex = "\"[^\"]+\"" :: Text
        chompQuotes = Text.init . Text.tail
        mkLiteral index text =
          StringLiteral
            { stringWords = wordMatch <$> Text.splitOn " " (chompQuotes text),
              stringIndex = fst index + 1, -- Don't include first quote
              stringLength = snd index - 2 -- Subtract surrounding quotes
            }
    wordMatch :: Text -> WordMatch
    wordMatch text
      | Set.member text tachyons = NoReplacement text
      | otherwise = Other

wordsInFile :: SourceFile -> [WordMatch]
wordsInFile sourceFile = concat $ stringWords <$> sourceFileStrings sourceFile

tachyonsInFile :: SourceFile -> [Text]
tachyonsInFile sourceFile =
  foldr className [] $ wordsInFile sourceFile
  where
    className word rs =
      case word of
        Other -> rs
        Replacement fromClassName _ -> fromClassName : rs
        NoReplacement className -> className : rs
