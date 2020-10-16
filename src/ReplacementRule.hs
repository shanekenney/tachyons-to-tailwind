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
          "fixed",
          "top-0",
          "right-0",
          "bottom-0",
          "left-0"
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
        ("h", "x"),
        ("v", "y"),
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
  [ Replace "fw1" "font-hairline",
    Replace "fw2" "font-thin",
    Replace "fw3" "font-light",
    Replace "fw4" "font-normal",
    Replace "normal" "font-normal",
    Replace "fw5" "font-medium",
    Replace "fw6" "font-semibold",
    Replace "fw7" "font-bold",
    Replace "b" "font-bold",
    Replace "fw8" "font-extrabold",
    Replace "fw9" "font-black"
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

border :: [Rule]
border =
  [ Replace "ba" "border",
    Replace "bt" "border-t",
    Replace "br" "border-r",
    Replace "bb" "border-b",
    Replace "bl" "border-l",
    Replace "bn" "border-0"
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
    Replace "br--left" "rounded-r-none",
    Replace "br-pill" "rounded-full"
  ]

fontSize :: [Rule]
fontSize =
  [ Replace "f1" "text-5xl",
    Replace "f2" "text-4xl",
    Replace "f3" "text-2xl",
    Replace "f4" "text-xl",
    Replace "f5" "text-base",
    Replace "f6" "text-sm",
    Replace "f7" "text-xs"
  ]

lineHeight :: [Rule]
lineHeight =
  [ Replace "lh-solid" "leading-4",
    Replace "lh-title" "leading-5",
    Replace "lh-copy" "leading-6"
  ]

height :: [Rule]
height =
  [ Replace "h-100" "h-full",
    Replace "vh-100" "h-screen"
  ]

width :: [Rule]
width =
  [ Replace "w-40" "w-2/5",
    Replace "w-60" "w-3/5",
    Replace "w-100" "w-full"
  ]

textAlign :: [Rule]
textAlign =
  [ Replace "tl" "text-left",
    Replace "tr" "text-right",
    Replace "tc" "text-center",
    Replace "tj" "text-justify"
  ]

colour :: [Rule]
colour =
  [ Replace "black" "text-black",
    Replace "white" "text-white"
  ]

other :: [Rule]
other =
  [ Keep "overflow-hidden",
    Keep "truncate",
    Replace "ttu" "uppercase"
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
          border,
          borderRadius,
          fontSize,
          lineHeight,
          height,
          width,
          textAlign,
          colour,
          other
        ]

getReplacement :: Text -> Maybe Text
getReplacement fromClass =
  Map.lookup fromClass rules
