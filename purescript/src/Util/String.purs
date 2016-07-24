module Util.String (isDigit, splice, whenJust) where

import Prelude
import Data.Maybe (Maybe(..))

-- yeah, i know this isn't a string function
isDigit :: Char -> Boolean
isDigit x = x >= '0' && x <= '9'

-- nor is this
whenJust :: forall a m. Monad m => Maybe a -> (a -> m Unit) -> m Unit
whenJust val f = case val of
  Just val' -> f val'
  Nothing -> pure unit

foreign import splice :: String -> Int -> Int -> String -> String
