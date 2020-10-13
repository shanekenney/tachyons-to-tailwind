module SourceFile
  ( SourceFile,
    readSourceFile,
    writeSourceFile,
    tachyonsInFile,
    replacementsInFile,
    noReplacementsInFile,
  )
where

import Data.Foldable (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import ReplacementRule (getReplacement)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype SourceFile = SourceFile [SourceContent]
  deriving (Show)

data SourceContent
  = StringLiteral [WordMatch]
  | SourceChunk Text
  deriving (Show)

data WordMatch
  = Other Text
  | Replacement Text Text
  | NoReplacement Text
  deriving (Show, Eq)

type Parser = Parsec Void Text

stringLiteralParser :: (Text -> WordMatch) -> Parser SourceContent
stringLiteralParser wordMatch =
  StringLiteral
    . fmap wordMatch
    . Text.splitOn " "
    <$> doubleQuoteStr
  where
    doubleQuoteStr =
      Text.pack
        <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

sourceChunkParser :: Parser SourceContent
sourceChunkParser =
  SourceChunk . Text.pack <$> many (anySingleBut '\"')

sourceFileParser :: (Text -> WordMatch) -> Parser SourceFile
sourceFileParser wordMatch =
  SourceFile
    <$> manyTill
      (stringLiteralWords <|> sourceChunkParser)
      eof
  where
    stringLiteralWords = stringLiteralParser wordMatch

readSourceFile :: (Text -> Bool) -> Text -> SourceFile
readSourceFile isMatch fileContent =
  fromMaybe (SourceFile []) $
    parseMaybe (sourceFileParser wordMatch) fileContent
  where
    wordMatch :: Text -> WordMatch
    wordMatch text =
      case getReplacement text of
        Just replacement ->
          Replacement text replacement
        Nothing ->
          if isMatch text
            then NoReplacement text
            else Other text

writeSourceFile :: SourceFile -> Text
writeSourceFile (SourceFile content) = foldl' toText Text.empty content
  where
    toText text (StringLiteral words') = text <> "\"" <> wordsToText words' <> "\""
    toText text (SourceChunk textChunk) = text <> textChunk

sourceFileStrings :: SourceFile -> [SourceContent]
sourceFileStrings (SourceFile content) =
  filter isStringLiteral content
  where
    isStringLiteral (StringLiteral _) = True
    isStringLiteral _ = False

stringWords :: SourceContent -> [WordMatch]
stringWords (StringLiteral words') = words'
stringWords _ = []

wordsToText :: [WordMatch] -> Text
wordsToText = Text.intercalate " " . fmap wordToText
  where
    wordToText :: WordMatch -> Text
    wordToText (Replacement _ to) = to
    wordToText (NoReplacement word) = word
    wordToText (Other word) = word

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
        Other _ -> results

replacementsInFile :: SourceFile -> [(Text, Text)]
replacementsInFile sourceFile =
  foldr collectReplacements [] $ wordsInFile sourceFile
  where
    collectReplacements word results =
      case word of
        Replacement from to -> (from, to) : results
        NoReplacement _ -> results
        Other _ -> results

noReplacementsInFile :: SourceFile -> [Text]
noReplacementsInFile sourceFile =
  foldr collectNoReplacements [] $ wordsInFile sourceFile
  where
    collectNoReplacements word results =
      case word of
        NoReplacement className -> className : results
        Replacement _ _ -> results
        Other _ -> results
