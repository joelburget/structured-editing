module Util.String (isDigit, spliceStr, spliceArr, whenJust, iFor, iForM) where

import Prelude
import Control.Monad.State (modify, get, evalState, evalStateT)
import Control.Monad.Trans (lift)
import Data.Maybe (Maybe(..))
import Data.Traversable

-- yeah, i know this isn't a string function
isDigit :: Char -> Boolean
isDigit x = x >= '0' && x <= '9'

-- nor is this
whenJust :: forall a m. Monad m => Maybe a -> (a -> m Unit) -> m Unit
whenJust val f = case val of
  Just val' -> f val'
  Nothing -> pure unit

foreign import spliceStr :: String -> Int -> Int -> String -> String
foreign import spliceArr :: forall a. Array a -> Int -> Int -> Array a -> Array a

iFor :: forall a b t. Traversable t => t a -> (Int -> a -> b) -> t b
iFor as f = flip evalState 0 $ flip traverse as \a -> do
  i <- get
  modify (_ + 1)
  pure (f i a)

iForM :: forall a b m t. (Traversable t, Monad m) => t a -> (Int -> a -> m b) -> m (t b)
iForM as f = flip evalStateT 0 $ flip traverse as \a -> do
  i <- get
  modify (_ + 1)
  lift $ f i a
