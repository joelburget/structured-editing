module Util.String (isDigit, splice) where

import Prelude ((>=), (<=), (&&))

-- yeah, i know this isn't a string function
isDigit :: Char -> Boolean
isDigit x = x >= '0' && x <= '9'

foreign import splice :: String -> Int -> Int -> String -> String
