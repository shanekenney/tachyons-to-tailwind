module ReplacementRule (getReplacement) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

data Rule
  = Replace Text Text
  | Keep Text

toPair :: Rule -> (Text, Text)
toPair (Replace from to) = (from, to)
toPair (Keep from) = (from, from)

spacingRules :: [Rule]
spacingRules =
  [ Replace "pa3" "p-4",
    Replace "pb3" "pb-4",
    Replace "pt3" "pt-4",
    Replace "pr3" "pr-4",
    Replace "pl3" "pl-4",
    Replace "pb0" "pb-0",
    Replace "ph3" "px-4",
    Replace "pv3" "py-4",
    Replace "ph4" "px-8"
  ]

rules :: Map Text Text
rules =
  Map.fromList $
    toPair
      <$> spacingRules
        <> [ Replace "fw6" "font-semibold",
             Replace "w-100" "w-full",
             Replace "f5" "text-base",
             Replace "db" "block",
             Keep "flex",
             Keep "top-0",
             Keep "overflow-hidden",
             Keep "justify-between"
           ]

getReplacement :: Text -> Maybe Text
getReplacement fromClass =
  Map.lookup fromClass rules
