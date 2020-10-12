module ReplacementRule (getReplacement) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

data Rule
  = Replace Text Text
  | Keep Text
  deriving (Show)

toPair :: Rule -> (Text, Text)
toPair (Replace from to) = (from, to)
toPair (Keep from) = (from, from)

position :: [Rule]
position =
  Keep
    <$> [ "static",
          "relative",
          "absolute",
          "fixed"
        ]

spacing :: [Rule]
spacing =
  [mkRule b m s | b <- base, m <- modifier, s <- scale]
  where
    mkRule :: Text -> (Text, Text) -> (Text, Text) -> Rule
    mkRule b m s =
      Replace
        (b <> fst m <> fst s)
        (b <> snd m <> "-" <> snd s)

    base :: [Text]
    base =
      [ "p",
        "m"
      ]

    modifier :: [(Text, Text)]
    modifier =
      [ ("a", ""),
        ("h", "y"),
        ("v", "x"),
        ("t", "t"),
        ("r", "r"),
        ("b", "b"),
        ("l", "l")
      ]

    scale :: [(Text, Text)]
    scale =
      [ ("0", "0"),
        ("1", "1"),
        ("2", "2"),
        ("3", "4"),
        ("4", "8"),
        ("5", "16"),
        ("6", "32"),
        ("7", "64")
      ]

other :: [Rule]
other =
  [ Replace "fw6" "font-semibold",
    Replace "w-100" "w-full",
    Replace "f5" "text-base",
    Replace "db" "block",
    Keep "flex",
    Keep "top-0",
    Keep "overflow-hidden",
    Keep "justify-between"
  ]

rules :: Map Text Text
rules =
  Map.fromList $
    toPair
      <$> mconcat
        [ spacing,
          position,
          other
        ]

getReplacement :: Text -> Maybe Text
getReplacement fromClass =
  Map.lookup fromClass rules
