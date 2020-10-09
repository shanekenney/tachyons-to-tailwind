module SourceFile
  ( SourceFile,
    readSourceFile,
    tachyonsInFile,
    replacementsInFile,
    noReplacementsInFile
  )
where

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
  foldr collectMatches [] $ wordsInFile sourceFile
  where
    collectMatches word results =
      case word of
        Replacement fromClassName _ -> fromClassName : results
        NoReplacement className -> className : results
        Other -> results

replacementsInFile :: SourceFile -> [(Text, Text)]
replacementsInFile sourceFile =
  foldr collectReplacements [] $ wordsInFile sourceFile
  where
    collectReplacements word results =
      case word of
        Replacement from to -> (from, to) : results
        NoReplacement _ -> results
        Other -> results

noReplacementsInFile :: SourceFile -> [Text]
noReplacementsInFile sourceFile =
  foldr collectNoReplacements [] $ wordsInFile sourceFile
  where
    collectNoReplacements word results =
      case word of
        NoReplacement className -> className : results
        Replacement _ _ -> results
        Other -> results
