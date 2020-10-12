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

fontWeight :: [Rule]
fontWeight =
  [ Replace "fw1" "hairline",
    Replace "fw2" "thin",
    Replace "fw3" "light",
    Replace "fw4" "normal",
    Keep "normal",
    Replace "fw5" "medium",
    Replace "fw6" "semibold",
    Replace "fw7" "bold",
    Replace "b" "bold",
    Replace "fw8" "extrabold",
    Replace "fw9" "black"
  ]

display :: [Rule]
display =
  [ Replace "dn" "hidden",
    Replace "di" "inline",
    Replace "db" "block",
    Replace "dib" "inline-block",
    Replace "dt" "table"
  ]

flex :: [Rule]
flex =
  [ Keep "flex",
    Keep "inline-flex",
    Keep "flex-auto",
    Keep "flex-none",
    Keep "items-start",
    Keep "items-end",
    Keep "items-center",
    Keep "items-baseline",
    Keep "items-stretch",
    Keep "justify-start",
    Keep "justify-end",
    Keep "justify-center",
    Keep "justify-between",
    Keep "justify-around",
    Keep "flex-row",
    Keep "flex-row-reverse",
    Replace "flex-column" "flex-col",
    Replace "flex-column-reverse" "flex-col-reverse"
  ]

borderRadius :: [Rule]
borderRadius =
  [ Replace "br0" "rounded-none",
    Replace "br1" "rounded-sm",
    Replace "br2" "rounded",
    Replace "br3" "rounded-lg",
    Replace "br4" "rounded-2xl",
    Replace "br--bottom" "rounded-t-none",
    Replace "br--top" "rounded-b-none",
    Replace "br--right" "rounded-l-none",
    Replace "br--left" "rounded-r-none"
  ]

other :: [Rule]
other =
  [ Replace "w-100" "w-full",
    Replace "f5" "text-base",
    Keep "top-0",
    Keep "overflow-hidden"
  ]

rules :: Map Text Text
rules =
  Map.fromList $
    toPair
      <$> mconcat
        [ spacing,
          position,
          fontWeight,
          display,
          flex,
          borderRadius,
          other
        ]

getReplacement :: Text -> Maybe Text
getReplacement fromClass =
  Map.lookup fromClass rules
