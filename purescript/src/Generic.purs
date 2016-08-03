module Generic where

import Data.Foreign.Generic (Options, SumEncoding(..), defaultOptions, readGeneric)

myOptions :: Options
myOptions = defaultOptions
  { sumEncoding = TaggedObject { tagFieldName: "tag", contentsFieldName: "value" }
  , unwrapNewtypes = true
  }
